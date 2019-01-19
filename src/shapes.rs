use std::collections::HashMap;

use crate::types::*;

pub type Link<L, C> = Option<Box<Shape<L, C>>>;

pub struct Shape<L, C> {
    pub id: ShapeId,
    pub kind: ShapeKind<L, C>,
    pub next: Link<L, C>,
}

pub enum ShapeKind<L, C> {
    Simple(SimpleShape<L, C>),
    Loop(LoopShape<L, C>),
    Multi(MultipleShape<L, C>),
}

pub struct SimpleShape<L, C> {
    pub internal: L,
    pub branches_out: HashMap<BlockId, ProcessedBranch<C>>,
}

pub struct LoopShape<L, C> {
    pub inner: Box<Shape<L, C>>,
}

pub struct MultipleShape<L, C> {
    pub handled: HashMap<BlockId, Shape<L, C>>,
}
