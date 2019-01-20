use crate::shapes::*;
use crate::types::{BlockId, FlowType, ShapeId};

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

    fn wrap_in_loop(self, shape_id: Option<ShapeId>) -> Self;

    fn wrap_in_block(self, shape_id: ShapeId) -> Self;

    fn handled(self, cond_type: CondType<&Self::Expr>) -> Self;
}

impl<E> Shape<E, E> {
    pub fn render<S: StructedAst<Expr = E>>(&self) -> S {
        let head: S = match &self.kind {
            ShapeKind::Simple(s) => s.render(),
            ShapeKind::Multi(s) => s.render(self.id),
            ShapeKind::Loop(s) => s.render(Some(self.id)),
            ShapeKind::Fused(s) => s.render(),
        };
        if let Some(ref next) = self.next {
            head.join(next.render())
        } else {
            head
        }
    }
}
impl<E> SimpleShape<E, E> {
    fn render<S: StructedAst<Expr = E>>(&self) -> S {
        let mut output: S = S::expr(&self.internal);

        for (is_first, b) in flag_first(self.conditional_branches()) {
            let cond_type = if is_first {
                CondType::If(b.data.as_ref().unwrap())
            } else {
                CondType::ElseIf(b.data.as_ref().unwrap())
            };
            let exit = match b.flow_type {
                FlowType::Direct => Exit {
                    set_label: Some(b.target),
                    flow: Flow::Direct,
                },
                FlowType::Continue => Exit {
                    set_label: Some(b.target),
                    flow: Flow::Continue(Some(b.ancestor)),
                },
                FlowType::Break => Exit {
                    set_label: Some(b.target),
                    flow: Flow::Break(Some(b.ancestor)),
                },
            };
            let exit = S::exit(exit).handled(cond_type);
            output = output.join(exit);
        }

        if let Some(b) = self.default_branch() {
            let has_multiple_targets = self.branches_out.len() > 1;
            let exit = match b.flow_type {
                FlowType::Direct => Exit {
                    set_label: Some(b.target),
                    flow: Flow::Direct,
                },
                FlowType::Continue => Exit {
                    set_label: Some(b.target),
                    flow: Flow::Continue(Some(b.ancestor)),
                },
                FlowType::Break => Exit {
                    set_label: Some(b.target),
                    flow: Flow::Break(Some(b.ancestor)),
                },
            };
            if has_multiple_targets {
                let cond_type: CondType<&E> = CondType::Else;
                let exit = S::exit(exit).handled(cond_type);
                output = output.join(exit);
            } else {
                let exit = S::exit(exit);
                output = output.join(exit);
            }
        } else {
            output = output.join(S::trap());
        }

        output
    }
}
impl<E> MultipleShape<E, E> {
    fn render<S: StructedAst<Expr = E>>(&self, shape_id: ShapeId) -> S {
        let mut body: Option<S> = None;

        for (is_first, (target, inner_shape)) in flag_first(self.handled.iter())
        {
            let cond_type: CondType<&E> = if is_first {
                CondType::IfLabel(*target)
            } else {
                CondType::ElseIfLabel(*target)
            };
            let inner: S = inner_shape.shape.render();
            let inner: S = inner.handled(cond_type);
            body = body.map(|body| body.join(inner));
        }

        if self.break_count > 0 {
            body.unwrap().wrap_in_block(shape_id)
        } else {
            body.unwrap()
        }
    }
}
impl<E> LoopShape<E, E> {
    fn render<S: StructedAst<Expr = E>>(&self, shape_id: Option<ShapeId>) -> S {
        let inner: S = self.inner.render();
        inner.wrap_in_loop(shape_id)
    }
}
impl<E> FusedShape<E, E> {
    fn render<S: StructedAst<Expr = E>>(&self) -> S {
        unimplemented!()
    }
}

fn flag_first<I: Iterator>(iter: I) -> impl Iterator<Item = (bool, I::Item)> {
    iter.enumerate().map(|(i, x)| (i == 0, x))
}

#[cfg(test)]
mod tests {
    use super::*;

    enum Ast {
        Panic,
        Node(String),
        If(String, Box<Ast>),
        ElseIf(String, Box<Ast>),
        Else(Box<Ast>),
        Loop(Option<usize>, Box<Ast>),
        Block(usize, Box<Ast>),
        Program(Vec<Ast>),
    }

    impl StructedAst for Ast {
        type Expr = str;

        fn trap() -> Self {
            Ast::Panic
        }

        fn expr(expr: &Self::Expr) -> Self {
            Ast::Node(expr.to_string())
        }

        fn exit(exit: Exit) -> Self {
            let label = if let Some(bid) = exit.set_label {
                format!("__label__ = {}\n", bid.index())
            } else {
                "\n".to_string()
            };

            match exit.flow {
                Flow::Direct => Ast::Node(label),
                Flow::Continue(Some(shape)) => {
                    let code = format!("{}continue 'L{}", label, shape.index());
                    Ast::Node(code)
                }
                Flow::Continue(None) => {
                    let code = format!("{}continue", label);
                    Ast::Node(code)
                }
                Flow::Break(Some(shape)) => {
                    let code = format!("{}break 'L{}", label, shape.index());
                    Ast::Node(code)
                }
                Flow::Break(None) => {
                    let code = format!("{}break", label);
                    Ast::Node(code)
                }
            }
        }

        fn join(mut self, other: Self) -> Self {
            match &mut self {
                Ast::Program(xs) => {
                    xs.push(other);
                    self
                }
                _ => Ast::Program(vec![self, other]),
            }
        }

        fn wrap_in_loop(self, shape_id: Option<ShapeId>) -> Self {
            let id = shape_id.map(|id| id.index());
            Ast::Loop(id, Box::new(self))
        }

        fn wrap_in_block(self, shape_id: ShapeId) -> Self {
            let id = shape_id.index();
            Ast::Block(id, Box::new(self))
        }

        fn handled(self, cond_type: CondType<&Self::Expr>) -> Self {
            match cond_type {
                CondType::If(c) => Ast::If(c.to_string(), Box::new(self)),
                CondType::ElseIf(c) => {
                    Ast::ElseIf(c.to_string(), Box::new(self))
                }
                CondType::IfLabel(bid) => Ast::If(
                    format!("__label__ == {}", bid.index()),
                    Box::new(self),
                ),
                CondType::ElseIfLabel(bid) => Ast::ElseIf(
                    format!("__label__ == {}", bid.index()),
                    Box::new(self),
                ),
                CondType::Else => Ast::Else(Box::new(self)),
            }
        }
    }

    #[test]
    fn test_it() {}
}
