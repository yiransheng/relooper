mod pretty_print;

use crate::{BlockId, CondType, Exit, Flow, Relooper, ShapeId, StructedAst};

#[derive(Debug)]
pub struct GenericCLike<F, G> {
    ast: Ast,
    loop_prefix: F,
    block_prefix: G,
}

#[derive(Debug)]
enum Ast {
    Panic,
    Node(String),
    If(String, Box<Ast>),
    ElseIf(String, Box<Ast>),
    Else(Box<Ast>),
    Loop(usize, Box<Ast>),
    Block(Option<usize>, Vec<Ast>),
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
            Ast::Block(_, xs) => {
                xs.push(other);
                self
            }
            _ => Ast::Block(None, vec![self, other]),
        }
    }

    fn wrap_in_loop(self, shape_id: ShapeId) -> Self {
        let id = shape_id.index();
        Ast::Loop(id, Box::new(self))
    }

    fn wrap_in_block(self, shape_id: ShapeId) -> Self {
        let id = shape_id.index();
        Ast::Block(Some(id), vec![self])
    }

    fn handled(self, cond_type: CondType<&Self::Expr>) -> Self {
        match cond_type {
            CondType::If(c) => Ast::If(c.to_string(), Box::new(self)),
            CondType::ElseIf(c) => Ast::ElseIf(c.to_string(), Box::new(self)),
            CondType::IfLabel(bid) => {
                Ast::If(format!("__label__ == {}", bid.index()), Box::new(self))
            }
            CondType::ElseIfLabel(bid) => Ast::ElseIf(
                format!("__label__ == {}", bid.index()),
                Box::new(self),
            ),
            CondType::Else => Ast::Else(Box::new(self)),
        }
    }
}
