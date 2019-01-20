use std::convert::AsRef;

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
    pub fn render<S: StructedAst>(&self) -> S
    where
        E: AsRef<S::Expr>,
    {
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
    fn render<S: StructedAst>(&self) -> S
    where
        E: AsRef<S::Expr>,
    {
        let mut output: S = S::expr(self.internal.as_ref());

        for (is_first, b) in flag_first(self.conditional_branches()) {
            let cond = b.data.as_ref().unwrap();
            let cond = cond.as_ref();
            let cond_type: CondType<&S::Expr> = if is_first {
                CondType::If(cond)
            } else {
                CondType::ElseIf(cond)
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
                    set_label: if has_multiple_targets {
                        Some(b.target)
                    } else {
                        None
                    },
                    flow: Flow::Continue(Some(b.ancestor)),
                },
                FlowType::Break => Exit {
                    set_label: if has_multiple_targets {
                        Some(b.target)
                    } else {
                        None
                    },
                    flow: Flow::Break(Some(b.ancestor)),
                },
            };
            if has_multiple_targets {
                let cond_type: CondType<&S::Expr> = CondType::Else;
                let exit = S::exit(exit).handled(cond_type);
                output = output.join(exit);
            } else {
                let exit = S::exit(exit);
                output = output.join(exit);
            }
            // } else {
            // output = output.join(S::trap());
        }

        output
    }
}
impl<E> MultipleShape<E, E> {
    fn render<S: StructedAst>(&self, shape_id: ShapeId) -> S
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
    fn render<S: StructedAst>(&self, shape_id: Option<ShapeId>) -> S
    where
        E: AsRef<S::Expr>,
    {
        let inner: S = self.inner.render();
        inner.wrap_in_loop(shape_id)
    }
}
impl<E> FusedShape<E, E> {
    fn render<S: StructedAst>(&self) -> S
    where
        E: AsRef<S::Expr>,
    {
        unimplemented!()
    }
}

fn flag_first<I: Iterator>(iter: I) -> impl Iterator<Item = (bool, I::Item)> {
    iter.enumerate().map(|(i, x)| (i == 0, x))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io;

    #[derive(Debug)]
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

    #[derive(Clone, Debug)]
    pub struct PrettyFormatter<'a> {
        current_indent: usize,
        indent: &'a str,
    }

    impl<'a> PrettyFormatter<'a> {
        fn write_node(&mut self, s: &str) -> Result<(), io::Error> {
            Ok(())
        }
    }

    fn indent<W: ?Sized>(wr: &mut W, n: usize, s: &[u8]) -> io::Result<()>
    where
        W: io::Write,
    {
        for _ in 0..n {
            wr.write_all(s)?;
        }

        Ok(())
    }

    #[test]
    fn test_it() {
        use crate::relooper::Relooper;
        let mut relooper: Relooper<String, String> = Relooper::new();

        let a = relooper.add_block("x = 0".to_string());
        let b = relooper.add_block("// test".to_string());
        let c = relooper.add_block("x = x + 1".to_string());
        let d = relooper.add_block("ok".to_string());

        relooper.add_branch(a, b, None);
        relooper.add_branch(b, c, None);
        relooper.add_branch(c, b, None);
        relooper.add_branch(b, d, Some("x >= 10".to_string()));

        let ast: Ast = relooper.render(a).unwrap();

        println!("{:#?}", ast);

        assert!(false);
    }
}
