use crate::types::*;

pub type Link<L, C> = Option<Box<Shape<L, C>>>;

pub struct Shape<L, C> {
    id: ShapeId,
    kind: ShapeKind<L, C>,
    next: Link<L, C>,
}

pub enum ShapeKind<L, C> {
    Simple(SimpleShape<L, C>),
    Loop(LoopShape<L, C>),
    Multi(MultipleShape<L, C>),
}

pub struct SimpleShape<L, C> {
    internal: L,
    branches_out: Vec<(BlockId, ProcessedBranch<C>)>,
}

pub struct LoopShape<L, C> {
    inner: Box<Shape<L, C>>,
}

pub struct MultipleShape<L, C> {
    handled: Vec<(BlockId, Shape<L, C>)>,
}
