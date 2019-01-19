use std::marker::PhantomData;
use std::ops::Index;

use fixedbitset::FixedBitSet;
use petgraph::graph::{DiGraph, EdgeIndex, IndexType, NodeIndex};

pub type DefaultIndex = u32;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ShapeId<Ix = DefaultIndex>(Ix);

pub struct ShapeIdGen {
    counter: usize,
}
impl ShapeIdGen {
    pub(crate) fn next_shape_id(&mut self) -> ShapeId {
        let sid = ShapeId::new(self.counter);
        self.counter += 1;
        sid
    }
}

impl<Ix: IndexType> ShapeId<Ix> {
    #[inline]
    pub(crate) fn new(index: usize) -> Self {
        ShapeId(Ix::new(index))
    }
    #[inline]
    pub fn index(&self) -> usize {
        self.0.index()
    }
}

pub(crate) type BlockId = NodeIndex<DefaultIndex>;

pub(crate) type BranchId = EdgeIndex<DefaultIndex>;

#[derive(Debug, Clone)]
pub(crate) struct BlockSet<Ix = BlockId> {
    inner: FixedBitSet,
    _idx_ty: PhantomData<Ix>,
}

impl<Ix: IndexType> BlockSet<NodeIndex<Ix>> {
    pub(crate) fn new_empty(size: usize) -> Self {
        BlockSet {
            inner: FixedBitSet::with_capacity(size),
            _idx_ty: PhantomData,
        }
    }
    // panics if index out of bound
    #[inline]
    pub(crate) fn insert(&mut self, idx: NodeIndex<Ix>) -> bool {
        self.inner.put(idx.index())
    }
    // panics if index out of bound
    #[inline]
    pub(crate) fn remove(&mut self, idx: NodeIndex<Ix>) {
        self.inner.set(idx.index(), false)
    }
    #[inline]
    pub(crate) fn clear(&mut self) {
        self.inner.clear();
    }
    pub(crate) fn contains(&self, idx: NodeIndex<Ix>) -> bool {
        self.inner[idx.index()]
    }
    #[inline]
    pub(crate) fn sample_one(&self) -> Option<NodeIndex<Ix>> {
        self.inner.ones().next().map(NodeIndex::new)
    }
    #[inline]
    pub(crate) fn take_one(&mut self) -> Option<NodeIndex<Ix>> {
        if let Some(i) = self.inner.ones().next() {
            self.inner.set(i, false);
            Some(NodeIndex::new(i))
        } else {
            None
        }
    }

    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.inner.count_ones(..)
    }

    pub(crate) fn iter<'a>(
        &'a self,
    ) -> impl Iterator<Item = NodeIndex<Ix>> + 'a {
        self.inner.ones().map(NodeIndex::new)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum FlowType {
    Direct,
    Break,
    Continue,
}

// All branches in input CFG will eventually be registered to
// some Simple shape
pub enum Branch<C> {
    // a branch has been registered to a leaf(simple) shape
    Registered,
    // unprocessed branch, with optional condition of type C
    Raw(Option<C>),
    // processed branch with known ancestor (loop/simple/multi)
    // but not yet registered to a simple shape
    Processed(ProcessedBranch<C>),
}

impl<C> Branch<C> {
    pub(crate) fn take(&mut self) -> Option<ProcessedBranch<C>> {
        let b = ::std::mem::replace(self, Branch::Registered);

        if let Branch::Processed(b) = b {
            Some(b)
        } else {
            *self = b;
            None
        }
    }

    pub(crate) fn solipsize(
        &mut self,
        target: BlockId,
        flow_type: FlowType,
        ancestor: ShapeId,
    ) {
        let b = ::std::mem::replace(self, Branch::Registered);

        if let Branch::Raw(data) = b {
            let processed = ProcessedBranch {
                data,
                flow_type,
                ancestor,
                target,
            };
            *self = Branch::Processed(processed);
        } else {
            *self = b;
        }
    }
}

pub struct ProcessedBranch<C> {
    pub data: Option<C>,
    pub flow_type: FlowType,
    pub ancestor: ShapeId,
    pub target: BlockId,
}

pub enum Block<L> {
    Raw(L),
    // has registered to a simple shape
    Registered,
}
impl<L> Block<L> {
    pub fn take(&mut self) -> Option<L> {
        let b = ::std::mem::replace(self, Block::Registered);
        if let Block::Raw(b) = b {
            Some(b)
        } else {
            *self = b;
            None
        }
    }
}

pub(crate) type CFGraph<L, C> = DiGraph<Block<L>, Branch<C>, DefaultIndex>;
