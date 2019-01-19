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
