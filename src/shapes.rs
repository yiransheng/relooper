use std::collections::HashMap;

use crate::types::*;

pub type Link<L, C> = Option<Box<Shape<L, C>>>;

#[derive(Debug)]
pub struct Shape<L, C> {
    pub id: ShapeId,
    pub kind: ShapeKind<L, C>,
    pub next: Link<L, C>,
}

#[derive(Debug)]
pub enum ShapeKind<L, C> {
    Simple(SimpleShape<L, C>),
    Loop(LoopShape<L, C>),
    Multi(MultipleShape<L, C>),
    Fused(FusedShape<L, C>),
}

#[derive(Debug)]
pub struct SimpleShape<L, C> {
    pub internal: L,
    pub branches_out: HashMap<BlockId, ProcessedBranch<C>>,
}

#[derive(Debug)]
pub struct LoopShape<L, C> {
    pub inner: Box<Shape<L, C>>,
}

#[derive(Debug, Clone, Copy)]
pub enum EntryType {
    Checked,
    Direct,
}

#[derive(Debug)]
pub struct HandledShape<L, C> {
    pub shape: Shape<L, C>,
    pub entry_type: EntryType,
}

#[derive(Debug)]
pub struct MultipleShape<L, C> {
    pub handled: HashMap<BlockId, HandledShape<L, C>>,
    pub break_count: usize,
}

#[derive(Debug)]
pub struct FusedShape<L, C> {
    simple: SimpleShape<L, C>,
    multi: MultipleShape<L, C>,
}

impl<L, C> Shape<L, C> {
    fn fuse(&mut self) {
        match &mut self.kind {
            ShapeKind::Simple(_) => self.fuse_simple(),
            ShapeKind::Loop(LoopShape { inner }) => inner.fuse(),
            ShapeKind::Multi(MultipleShape { handled, .. }) => {
                for (_, s) in handled.iter_mut() {
                    s.shape.fuse();
                }
            }
            ShapeKind::Fused(_) => {}
        }
    }

    fn fuse_simple(&mut self) {
        match &mut self.kind {
            ShapeKind::Simple(_) => {}
            _ => return,
        };
        match self.next.as_ref().map(|s| &s.kind) {
            Some(ShapeKind::Multi(_)) => {}
            _ => return,
        };
        let mut next = self.next.take().unwrap();
        next.fuse();
        self.next = next.next.take();

        take_mut::take(&mut self.kind, |kind| {
            if let ShapeKind::Simple(simple) = kind {
                if let ShapeKind::Multi(multi) = next.kind {
                    return ShapeKind::Fused(FusedShape { simple, multi });
                }
            }

            unreachable!()
        });
    }
}
