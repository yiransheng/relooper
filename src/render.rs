use std::convert::AsRef;

use crate::shapes::*;
use crate::types::{BlockId, EntryType, FlowType, ProcessedBranch, ShapeId};

#[derive(Debug)]
pub enum CondType<C> {
    Case(C),
    CaseLabel(BlockId),
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
    type Stmt: ?Sized;

    fn trap() -> Self;

    fn nop() -> Self;

    fn statement(stmt: &Self::Stmt) -> Self;

    fn exit(b: Exit) -> Self;

    fn join(self, other: Self) -> Self;

    fn wrap_in_loop(self, shape_id: ShapeId) -> Self;

    fn wrap_in_block(self, shape_id: ShapeId) -> Self;

    fn switches<'a, I: Iterator<Item = (CondType<&'a Self::Expr>, Self)>>(
        conditionals: I,
        default_branch: Self,
    ) -> Self
    where
        Self: Sized,
        Self::Expr: 'a;
}

impl<L, C> Shape<L, C> {
    pub fn render<S: StructedAst>(&self, root: &Self) -> S
    where
        C: AsRef<S::Expr>,
        L: AsRef<S::Stmt>,
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
impl<L, C> SimpleShape<L, C> {
    fn render_branch<S: StructedAst>(
        &self,
        branch: &ProcessedBranch<C>,
        fused_multi: Option<&MultipleShape<L, C>>,
        root: &Shape<L, C>,
    ) -> S
    where
        C: AsRef<S::Expr>,
        L: AsRef<S::Stmt>,
    {
        if let Some(handled_shape) =
            fused_multi.and_then(|s| s.handled.get(&branch.target))
        {
            assert!(branch.flow_type == FlowType::Direct);
            assert!(handled_shape.entry_type != EntryType::Checked);
            handled_shape.shape.render(root)
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
                        //// Early Return
                        return S::nop();
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

            S::exit(exit)
        }
    }
    fn render<S: StructedAst>(
        &self,
        fused_multi: Option<&MultipleShape<L, C>>,
        root: &Shape<L, C>,
    ) -> S
    where
        C: AsRef<S::Expr>,
        L: AsRef<S::Stmt>,
    {
        let mut output: S = S::statement(self.internal.as_ref());

        let has_conditional_branches = self.conditional_branches().count() > 0;

        let conditional_exits = self.conditional_branches().map(|b| {
            let exit: S = self.render_branch(b, fused_multi, root);
            let cond = b.data.as_ref().unwrap();
            let cond = cond.as_ref();
            let cond_type: CondType<&S::Expr> = CondType::Case(cond);

            (cond_type, exit)
        });

        let default_exit = self.default_branch().map(|b| {
            let exit: S = self.render_branch(b, fused_multi, root);
            exit
        });

        let exits = if has_conditional_branches {
            S::switches(
                conditional_exits,
                default_exit.unwrap_or_else(|| S::trap()),
            )
        } else {
            default_exit.unwrap_or_else(|| S::nop())
        };

        output.join(exits)
    }
}
impl<L, C> MultipleShape<L, C> {
    fn render<S: StructedAst>(&self, shape_id: ShapeId, root: &Shape<L, C>) -> S
    where
        L: AsRef<S::Stmt>,
        C: AsRef<S::Expr>,
    {
        let conditionals = self.handled.iter().map(|(target, inner_shape)| {
            let cond_type: CondType<&S::Expr> = CondType::CaseLabel(*target);
            let inner: S = inner_shape.shape.render(root);

            (cond_type, inner)
        });

        let body = S::switches(conditionals, S::nop());

        if self.break_count > 0 {
            body.wrap_in_block(shape_id)
        } else {
            body
        }
    }
}
impl<L, C> LoopShape<L, C> {
    fn render<S: StructedAst>(&self, shape_id: ShapeId, root: &Shape<L, C>) -> S
    where
        L: AsRef<S::Stmt>,
        C: AsRef<S::Expr>,
    {
        let inner: S = self.inner.render(root);
        inner.wrap_in_loop(shape_id)
    }
}
impl<L, C> FusedShape<L, C> {
    fn render<S: StructedAst>(&self, shape_id: ShapeId, root: &Shape<L, C>) -> S
    where
        L: AsRef<S::Stmt>,
        C: AsRef<S::Expr>,
    {
        let inner: S = self.simple.render(Some(&self.multi), root);
        if self.multi.break_count > 0 {
            inner.wrap_in_block(shape_id)
        } else {
            inner
        }
    }
}
