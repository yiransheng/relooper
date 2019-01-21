use std::convert::AsRef;

use crate::shapes::*;
use crate::types::{BlockId, EntryType, FlowType, ProcessedBranch, ShapeId};

#[derive(Debug)]
pub enum CondType<C> {
    If(C),
    ElseIf(C),
    IfLabel(BlockId),
    ElseIfLabel(BlockId),
    Else,
}

#[derive(Debug, Copy, Clone)]
pub struct Exit {
    pub set_label: Option<BlockId>,
    pub flow: Flow,
}
#[derive(Debug, Copy, Clone)]
pub enum Flow {
    Direct,
    Break(Option<ShapeId>),
    Continue(Option<ShapeId>),
}

pub trait StructedAst {
    type Expr: ?Sized;

    fn trap() -> Self;

    fn expr(expr: &Self::Expr) -> Self;

    fn exit(b: Exit) -> Self;

    fn join(self, other: Self) -> Self;

    fn wrap_in_loop(self, shape_id: ShapeId) -> Self;

    fn wrap_in_block(self, shape_id: ShapeId) -> Self;

    fn handled(self, cond_type: CondType<&Self::Expr>) -> Self;
}

impl<E> Shape<E, E> {
    pub fn render<S: StructedAst>(&self, root: &Self) -> S
    where
        E: AsRef<S::Expr>,
    {
        let head: S = match &self.kind {
            ShapeKind::Simple(s) => s.render(None, root),
            ShapeKind::Multi(s) => s.render(self.id, root),
            ShapeKind::Loop(s) => s.render(self.id, root),
            ShapeKind::Fused(s) => s.render(self.id, root),
        };
        if let Some(ref next) = self.next {
            head.join(next.render(root))
        } else {
            head
        }
    }
}
impl<E> SimpleShape<E, E> {
    fn render_branch<S: StructedAst>(
        &self,
        branch: &ProcessedBranch<E>,
        fused_multi: Option<&MultipleShape<E, E>>,
        root: &Shape<E, E>,
    ) -> Option<S>
    where
        E: AsRef<S::Expr>,
    {
        if let Some(handled_shape) =
            fused_multi.and_then(|s| s.handled.get(&branch.target))
        {
            assert!(branch.flow_type == FlowType::Direct);
            assert!(handled_shape.entry_type != EntryType::Checked);
            Some(handled_shape.shape.render(root))
        } else {
            let target_entry_type = find_branch_target_entry_type(branch, root);
            let set_label = Some(branch.target)
                .filter(|_| target_entry_type == EntryType::Checked);

            let exit = match branch.flow_type {
                FlowType::Direct => {
                    if set_label.is_some() {
                        Exit {
                            set_label,
                            flow: Flow::Direct,
                        }
                    } else {
                        return None;
                    }
                }
                FlowType::Continue => Exit {
                    set_label,
                    flow: Flow::Continue(Some(branch.ancestor)),
                },
                FlowType::Break => Exit {
                    set_label,
                    flow: Flow::Break(Some(branch.ancestor)),
                },
            };
            Some(S::exit(exit))
        }
    }
    fn render<S: StructedAst>(
        &self,
        fused_multi: Option<&MultipleShape<E, E>>,
        root: &Shape<E, E>,
    ) -> S
    where
        E: AsRef<S::Expr>,
    {
        let mut output: S = S::expr(self.internal.as_ref());

        let exits = self.conditional_branches().filter_map(|b| {
            let exit: Option<S> = self.render_branch(b, fused_multi, root);
            exit.map(|e| (b, e))
        });

        let mut has_conditional_branches = false;

        for (is_first, (b, exit)) in flag_first(exits) {
            has_conditional_branches = true;

            let cond = b.data.as_ref().unwrap();
            let cond = cond.as_ref();
            let cond_type: CondType<&S::Expr> = if is_first {
                CondType::If(cond)
            } else {
                CondType::ElseIf(cond)
            };
            let exit = exit.handled(cond_type);

            output = output.join(exit);
        }

        if let Some(b) = self.default_branch() {
            let exit: Option<S> = self.render_branch(b, fused_multi, root);

            if let Some(exit) = exit {
                if has_conditional_branches {
                    let cond_type: CondType<&S::Expr> = CondType::Else;
                    let exit = exit.handled(cond_type);
                    output = output.join(exit);
                } else {
                    output = output.join(exit);
                }
            }
        } else if has_conditional_branches {
            output = output.join(S::trap());
        }

        output
    }
}
impl<E> MultipleShape<E, E> {
    fn render<S: StructedAst>(&self, shape_id: ShapeId, root: &Shape<E, E>) -> S
    where
        E: AsRef<S::Expr>,
    {
        let mut body: Option<S> = None;

        for (is_first, (target, inner_shape)) in flag_first(self.handled.iter())
        {
            let cond_type: CondType<&S::Expr> = if is_first {
                CondType::IfLabel(*target)
            } else {
                CondType::ElseIfLabel(*target)
            };
            let inner: S = inner_shape.shape.render(root);
            let inner: S = inner.handled(cond_type);
            if body.is_none() {
                body = Some(inner);
            } else {
                body = body.map(|body| body.join(inner));
            }
        }

        if self.break_count > 0 {
            body.unwrap().wrap_in_block(shape_id)
        } else {
            body.unwrap()
        }
    }
}
impl<E> LoopShape<E, E> {
    fn render<S: StructedAst>(&self, shape_id: ShapeId, root: &Shape<E, E>) -> S
    where
        E: AsRef<S::Expr>,
    {
        let inner: S = self.inner.render(root);
        inner.wrap_in_loop(shape_id)
    }
}
impl<E> FusedShape<E, E> {
    fn render<S: StructedAst>(&self, shape_id: ShapeId, root: &Shape<E, E>) -> S
    where
        E: AsRef<S::Expr>,
    {
        let inner: S = self.simple.render(Some(&self.multi), root);
        if self.multi.break_count > 0 {
            inner.wrap_in_block(shape_id)
        } else {
            inner
        }
    }
}

fn flag_first<I: Iterator>(iter: I) -> impl Iterator<Item = (bool, I::Item)> {
    iter.enumerate().map(|(i, x)| (i == 0, x))
}
