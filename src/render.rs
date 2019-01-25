use std::borrow::{Borrow, ToOwned};
use std::iter;

use crate::shapes::*;
use crate::types::{BlockId, EntryType, FlowType, ProcessedBranch, ShapeId};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum CondType<C> {
    Case(C),
    CaseLabel(BlockId),
}
impl<C: Clone> CondType<&C> {
    pub fn cloned(&self) -> CondType<C> {
        match self {
            CondType::Case(c) => CondType::Case((*c).clone()),
            CondType::CaseLabel(bid) => CondType::CaseLabel(*bid),
        }
    }
}

// primarily for Cond<&str> type usage, method named `as_owned`
// because `to_owned` is already implemented due to derive(Clone)
impl<C: ToOwned + ?Sized> CondType<&C> {
    pub fn as_owned(&self) -> CondType<C::Owned> {
        match self {
            CondType::Case(c) => CondType::Case((*c).to_owned()),
            CondType::CaseLabel(bid) => CondType::CaseLabel(*bid),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Exit {
    pub set_label: Option<BlockId>,
    pub flow: Flow,
}
#[derive(Debug, Copy, Clone)]
pub enum Flow {
    Direct,
    Break(ShapeId),
    Continue(ShapeId),
}

pub trait StructuredAst {
    type Expr: ?Sized;
    type Stmt: ?Sized;

    fn merge<I>(nodes: I) -> Self
    where
        Self: Sized,
        I: Iterator<Item = Self>;

    fn trap() -> Self;

    fn statement(stmt: &Self::Stmt) -> Self;

    fn exit(b: Exit) -> Self;

    fn wrap_in_loop(self, shape_id: ShapeId) -> Self;

    fn wrap_in_block(self, shape_id: ShapeId) -> Self;

    fn switches<'a, I: Iterator<Item = (CondType<&'a Self::Expr>, Self)>>(
        conditionals: I,
        default_branch: Option<Self>,
    ) -> Self
    where
        Self: Sized,
        Self::Expr: 'a;
}

macro_rules! boxed_iter {
    ($e: expr) => {
        Box::new(iter::once($e))
    };
    ($x: expr, ..$y: expr) => {
        Box::new(iter::once($x).chain($y))
    };
    ($x: expr, $y: expr) => {{
        Box::new(iter::once($x).chain(iter::once($y)))
    }};
}

impl<L, C> Shape<L, C> {
    pub fn render<S: StructuredAst>(&self, root: &Self) -> S
    where
        C: Borrow<S::Expr>,
        L: Borrow<S::Stmt>,
        S: 'static,
    {
        S::merge(self.render_iter(root))
    }
    fn render_iter<'a, S: StructuredAst>(
        &self,
        root: &Self,
    ) -> Box<dyn Iterator<Item = S>>
    where
        C: Borrow<S::Expr>,
        L: Borrow<S::Stmt>,
        S: 'static,
    {
        let head = match &self.kind {
            ShapeKind::Simple(s) => s.render_iter(None, root),
            ShapeKind::Multi(s) => s.render_iter(self.id, root),
            ShapeKind::Loop(s) => s.render_iter(self.id, root),
            ShapeKind::Fused(s) => s.render_iter(self.id, root),
        };
        if let Some(ref next) = self.next {
            let iter = next.render_iter(root);
            Box::new(head.chain(iter))
        } else {
            head
        }
    }
}
impl<L, C> SimpleShape<L, C> {
    fn render_branch<S: StructuredAst>(
        &self,
        branch: &ProcessedBranch<C>,
        fused_multi: Option<&MultipleShape<L, C>>,
        root: &Shape<L, C>,
    ) -> Box<dyn Iterator<Item = S>>
    where
        C: Borrow<S::Expr>,
        L: Borrow<S::Stmt>,
        S: 'static,
    {
        if let Some(handled_shape) =
            fused_multi.and_then(|s| s.handled.get(&branch.target))
        {
            assert!(branch.flow_type == FlowType::Direct);
            assert!(handled_shape.entry_type != EntryType::Checked);
            handled_shape.shape.render_iter(root)
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
                        return Box::new(iter::empty());
                    }
                }
                FlowType::Continue => Exit {
                    set_label,
                    flow: Flow::Continue(branch.ancestor),
                },
                FlowType::Break => Exit {
                    set_label,
                    flow: Flow::Break(branch.ancestor),
                },
            };

            boxed_iter!(S::exit(exit))
        }
    }
    fn render_iter<S: StructuredAst>(
        &self,
        fused_multi: Option<&MultipleShape<L, C>>,
        root: &Shape<L, C>,
    ) -> Box<dyn Iterator<Item = S>>
    where
        C: Borrow<S::Expr>,
        L: Borrow<S::Stmt>,
        S: 'static,
    {
        let output: S = S::statement(self.internal.borrow());

        let has_conditional_branches = self.conditional_branches().count() > 0;

        let conditional_exits = self.conditional_branches().map(|b| {
            let exit: S = S::merge(self.render_branch(b, fused_multi, root));
            let cond = b.data.as_ref().unwrap();
            let cond = cond.borrow();
            let cond_type: CondType<&S::Expr> = CondType::Case(cond);

            (cond_type, exit)
        });

        let default_exit = self
            .default_branch()
            .map(|b| self.render_branch(b, fused_multi, root));

        if has_conditional_branches {
            let exits = S::switches(
                conditional_exits,
                default_exit.map(S::merge).or_else(|| Some(S::trap())),
            );
            boxed_iter!(output, exits)
        } else if let Some(exit) = default_exit {
            boxed_iter!(output, ..exit)
        } else {
            boxed_iter!(output)
        }
    }
}
impl<L, C> MultipleShape<L, C> {
    fn render_iter<S: StructuredAst>(
        &self,
        shape_id: ShapeId,
        root: &Shape<L, C>,
    ) -> Box<dyn Iterator<Item = S>>
    where
        L: Borrow<S::Stmt>,
        C: Borrow<S::Expr>,
        S: 'static,
    {
        let conditionals = self.handled.iter().map(|(target, inner_shape)| {
            let cond_type: CondType<&S::Expr> = CondType::CaseLabel(*target);
            let inner: S = inner_shape.shape.render(root);

            (cond_type, inner)
        });

        let body = S::switches(conditionals, None);

        if self.break_count > 0 {
            boxed_iter!(body.wrap_in_block(shape_id))
        } else {
            boxed_iter!(body)
        }
    }
}
impl<L, C> LoopShape<L, C> {
    fn render_iter<S: StructuredAst>(
        &self,
        shape_id: ShapeId,
        root: &Shape<L, C>,
    ) -> Box<dyn Iterator<Item = S>>
    where
        L: Borrow<S::Stmt>,
        C: Borrow<S::Expr>,
        S: 'static,
    {
        let inner: S = self.inner.render(root);
        boxed_iter!(inner.wrap_in_loop(shape_id))
    }
}
impl<L, C> FusedShape<L, C> {
    fn render_iter<S: StructuredAst>(
        &self,
        shape_id: ShapeId,
        root: &Shape<L, C>,
    ) -> Box<dyn Iterator<Item = S>>
    where
        L: Borrow<S::Stmt>,
        C: Borrow<S::Expr>,
        S: 'static,
    {
        let inner = self.simple.render_iter(Some(&self.multi), root);
        let inner = S::merge(inner);

        boxed_iter!(if self.multi.break_count > 0 {
            inner.wrap_in_block(shape_id)
        } else {
            inner
        })
    }
}

struct TwoItems<T> {
    left: Option<T>,
    right: Option<T>,
}
impl<T> TwoItems<T> {
    #[inline]
    fn boxed(left: T, right: T) -> Box<Self> {
        Box::new(TwoItems {
            left: Some(left),
            right: Some(right),
        })
    }
}

impl<T> Iterator for TwoItems<T> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<T> {
        self.left.take().or_else(|| self.right.take())
    }
}
