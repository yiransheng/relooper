mod pretty_print;

use std::marker::PhantomData;

use crate::{CondType, Exit, Flow, ShapeId, StructedAst};

#[derive(Debug, Eq, PartialEq)]
pub struct CLikeAst<C = DefaultConfig> {
    kind: AstKind,
    _config: PhantomData<C>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct DefaultConfig;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct RustConfig;

pub trait StaticAstConfig {
    fn config() -> AstConfig<'static>;
}

#[derive(Debug, Copy, Clone)]
pub struct AstConfig<'a> {
    pub label_prefix: &'a str,
    pub loop_prefix: &'a str,
    pub loop_postfix: &'a str,
    pub labed_block_prefix: &'a str,
    pub labed_block_postfix: &'a str,
    pub control_variable: &'a str,
    pub panic: &'a str,
}

impl<'a> Default for AstConfig<'a> {
    fn default() -> Self {
        AstConfig {
            label_prefix: "L",
            loop_prefix: "while (true) {",
            loop_postfix: "}",
            labed_block_prefix: "do {",
            labed_block_postfix: "} while(false)",
            control_variable: "__label__",
            panic: "throw Error('unreachable');",
        }
    }
}

impl StaticAstConfig for DefaultConfig {
    fn config() -> AstConfig<'static> {
        AstConfig::default()
    }
}

impl StaticAstConfig for RustConfig {
    fn config() -> AstConfig<'static> {
        AstConfig {
            label_prefix: "'a",
            loop_prefix: "loop {",
            loop_postfix: "}",
            labed_block_prefix: "loop {{",
            labed_block_postfix: "} break; }",
            control_variable: "__label__",
            panic: "unreachable!()",
        }
    }
}

impl<C: StaticAstConfig> From<AstKind> for CLikeAst<C> {
    fn from(kind: AstKind) -> Self {
        CLikeAst {
            kind,
            _config: PhantomData,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum AstKind {
    Panic,
    Node(String),
    If(String, Box<AstKind>),
    ElseIf(String, Box<AstKind>),
    Else(Box<AstKind>),
    Loop(usize, Box<AstKind>),
    LabeledBlock(usize, Box<AstKind>),
    Seq(Vec<AstKind>),
}

impl<C: StaticAstConfig> StructedAst for CLikeAst<C> {
    type Expr = str;

    fn trap() -> Self {
        AstKind::Panic.into()
    }

    fn expr(expr: &Self::Expr) -> Self {
        AstKind::Node(expr.to_string()).into()
    }

    fn exit(exit: Exit) -> Self {
        let c = C::config();

        let label = if let Some(bid) = exit.set_label {
            format!("{} = {};\n", c.control_variable, bid.index())
        } else {
            "".to_string()
        };

        match exit.flow {
            Flow::Direct => AstKind::Node(label).into(),
            Flow::Continue(Some(shape)) => {
                let code = format!(
                    "{}continue {}{};",
                    label,
                    c.label_prefix,
                    shape.index()
                );
                AstKind::Node(code).into()
            }
            Flow::Continue(None) => {
                let code = format!("{}continue;", label);
                AstKind::Node(code).into()
            }
            Flow::Break(Some(shape)) => {
                let code = format!(
                    "{}break {}{};",
                    label,
                    c.label_prefix,
                    shape.index()
                );
                AstKind::Node(code).into()
            }
            Flow::Break(None) => {
                let code = format!("{}break;", label);
                AstKind::Node(code).into()
            }
        }
    }

    fn join(mut self, mut other: Self) -> Self {
        // inefficient
        match &mut other.kind {
            AstKind::Seq(xs) => {
                let mut ys = Vec::with_capacity(xs.len() + 1);
                ys.push(self.kind);
                ys.extend(xs.drain(..));
                return AstKind::Seq(ys).into();
            }
            _ => {}
        }

        match &mut self.kind {
            AstKind::Seq(xs) => {
                xs.push(other.kind);
                self
            }
            _ => AstKind::Seq(vec![self.kind, other.kind]).into(),
        }
    }

    fn wrap_in_loop(self, shape_id: ShapeId) -> Self {
        let id = shape_id.index();
        AstKind::Loop(id, Box::new(self.kind)).into()
    }

    fn wrap_in_block(self, shape_id: ShapeId) -> Self {
        let id = shape_id.index();
        AstKind::LabeledBlock(id, Box::new(self.kind)).into()
    }

    fn handled(self, cond_type: CondType<&Self::Expr>) -> Self {
        let c = C::config();

        match cond_type {
            CondType::If(c) => {
                AstKind::If(c.to_string(), Box::new(self.kind)).into()
            }
            CondType::ElseIf(c) => {
                AstKind::ElseIf(c.to_string(), Box::new(self.kind)).into()
            }
            CondType::IfLabel(bid) => AstKind::If(
                format!("{} == {}", c.control_variable, bid.index()),
                Box::new(self.kind),
            )
            .into(),
            CondType::ElseIfLabel(bid) => AstKind::ElseIf(
                format!("{} == {}", c.control_variable, bid.index()),
                Box::new(self.kind),
            )
            .into(),
            CondType::Else => AstKind::Else(Box::new(self.kind)).into(),
        }
    }
}
