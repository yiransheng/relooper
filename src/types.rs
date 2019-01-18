use std::marker::PhantomData;
use std::ops::Index;

use fixedbitset::FixedBitSet;
use petgraph::graph::{DiGraph, EdgeIndex, IndexType, NodeIndex};

type DefaultIndex = u32;

pub struct ShapeId<Ix = DefaultIndex>(Ix);

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

pub(crate) struct BlockSet<Ix = BlockId> {
    inner: FixedBitSet,
    _idx_ty: PhantomData<Ix>,
}

impl<Ix: IndexType> BlockSet<Ix> {
    // panics if index out of bound
    #[inline]
    pub(crate) fn insert(&mut self, idx: Ix) -> bool {
        self.inner.put(idx.index())
    }
    // panics if index out of bound
    #[inline]
    pub(crate) fn remove(&mut self, idx: Ix) {
        self.inner.set(idx.index(), false)
    }

    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.inner.count_ones(..)
    }
}
impl<Ix: IndexType> Index<Ix> for BlockSet<Ix> {
    type Output = bool;

    #[inline]
    fn index(&self, idx: Ix) -> &bool {
        self.inner.index(idx.index())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum FlowType {
    Direction,
    Break,
    Continue,
}

pub enum Branch<C> {
    // a branch has been registered to a leaf(simple) shape
    Registered,
    // unprocessed branch, with optional condition of type C
    Raw(Option<C>),
    // processed branch with known ancestor (loop/simple/multi)
    // but not yet registered to a simple shape
    Processed(ProcessedBranch<C>),
}

pub struct ProcessedBranch<C> {
    pub data: Option<C>,
    pub flow_type: FlowType,
    pub ancestor: ShapeId,
    pub target: BlockId,
}

pub(crate) type CFGraph<L, C> = DiGraph<L, Branch<C>, DefaultIndex>;
