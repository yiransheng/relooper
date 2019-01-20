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
    pub block_id: BlockId,
    pub internal: L,
    pub branches_out: HashMap<BlockId, ProcessedBranch<C>>,
}

#[derive(Debug)]
pub struct LoopShape<L, C> {
    pub inner: Box<Shape<L, C>>,
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
    pub simple: SimpleShape<L, C>,
    pub multi: MultipleShape<L, C>,
}

impl<L, C> SimpleShape<L, C> {
    pub fn default_branch(&self) -> Option<&ProcessedBranch<C>> {
        let mut branches =
            self.branches_out.values().filter(|b| b.data.is_none());
        let ret = branches.next();

        assert!(branches.count() == 0, "More than one default branch");

        ret
    }

    pub fn conditional_branches<'a>(
        &'a self,
    ) -> impl Iterator<Item = &ProcessedBranch<C>> + 'a {
        self.branches_out.values().filter(|b| b.data.is_some())
    }
}

pub fn find_branch_target_entry_type<L, C>(
    branch: &ProcessedBranch<C>,
    root_shape: &Shape<L, C>,
) -> EntryType {
    root_shape
        .find_shape(branch.ancestor)
        .and_then(|shape| match branch.flow_type {
            FlowType::Continue => shape.target_entry_type(branch.target),
            _ => shape
                .next
                .as_ref()
                .and_then(|s| s.target_entry_type(branch.target)),
        })
        .unwrap_or(EntryType::Checked)
}

impl<L, C> Shape<L, C> {
    pub fn fuse(&mut self) {
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
        let new_id = next.id;
        self.id = new_id;
        self.next = next.next.take();

        take_mut::take(&mut self.kind, |kind| {
            if let ShapeKind::Simple(mut simple) = kind {
                if let ShapeKind::Multi(multi) = next.kind {
                    for b in simple.branches_out.values_mut() {
                        b.ancestor = new_id;
                        // not handled
                        if !multi.handled.contains_key(&b.target) {
                            b.flow_type = FlowType::Break;
                        }
                    }
                    return ShapeKind::Fused(FusedShape { simple, multi });
                }
            }

            unreachable!()
        });
    }

    fn find_shape(&self, id: ShapeId) -> Option<&Self> {
        if self.id == id {
            return Some(self);
        }
        match &self.kind {
            ShapeKind::Loop(LoopShape { inner }) => {
                if let Some(s) = inner.find_shape(id) {
                    return Some(s);
                }
            }
            ShapeKind::Multi(shape) => {
                for (_, shape) in shape.handled.iter() {
                    if let Some(s) = shape.shape.find_shape(id) {
                        return Some(s);
                    }
                }
            }
            ShapeKind::Fused(fused) => {
                for (_, shape) in fused.multi.handled.iter() {
                    if let Some(s) = shape.shape.find_shape(id) {
                        return Some(s);
                    }
                }
            }
            _ => {}
        }

        self.next.as_ref().and_then(|s| s.find_shape(id))
    }

    fn target_entry_type(&self, target: BlockId) -> Option<EntryType> {
        // Returning Some(EntryType::Checked) is always correct here
        //
        // However, we would like to identify situations EntryType::Direct
        // Can be used. In render type, this avoids setting label for
        // control flow. &self should be the ancestor or immediate successor
        // of the ancestor depends on flow type (see pub fn above)
        //
        // Any ancestor -.next-> Simple: Direct
        // Any ancestor -.next-> Loop { inner }: recurse(inner, target)
        // Any ancestor -.next-> Multi handled : Direct if "fused", Checked otherwise
        // Any ancestor -.next-> Multi -> ... -> target: Checked (not handled in multi)

        match &self.kind {
            ShapeKind::Simple(s) if s.block_id == target => {
                return Some(EntryType::Direct);
            }
            ShapeKind::Simple(_) => {}
            ShapeKind::Loop(LoopShape { inner }) => {
                if let Some(t) = inner.target_entry_type(target) {
                    return Some(t);
                }
            }
            ShapeKind::Multi(shape) => {
                for (t, shape) in shape.handled.iter() {
                    if *t == target {
                        return Some(shape.entry_type);
                    }
                }
                // not handled
                return Some(EntryType::Checked);
            }
            ShapeKind::Fused(fused) => {
                if fused.simple.block_id == target {
                    return Some(EntryType::Direct);
                }

                for (t, shape) in fused.multi.handled.iter() {
                    if *t == target {
                        return Some(shape.entry_type);
                    }
                }
                // no handled
                return Some(EntryType::Checked);
            }
        }

        self.next.as_ref().and_then(|s| s.target_entry_type(target))
    }
}
