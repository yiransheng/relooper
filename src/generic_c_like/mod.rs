mod pretty_print;

use std::marker::PhantomData;

use crate::{CondType, Exit, Flow, ShapeId, StructuredAst};

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

impl<C: StaticAstConfig> StructuredAst for CLikeAst<C> {
    type Expr = str;
    type Stmt = str;

    fn merge<I>(nodes: I) -> Self
    where
        I: Iterator<Item = Self>,
    {
        let mut nodes: Vec<_> = nodes
            .filter_map(|node| match node.kind {
                AstKind::Seq(ref xs) if xs.is_empty() => None,
                _ => Some(node.kind),
            })
            .flat_map(|kind| match kind {
                AstKind::Seq(xs) => xs.into_iter(),
                _ => vec![kind].into_iter(),
            })
            .collect();

        if nodes.len() == 1 {
            nodes.pop().unwrap().into()
        } else {
            AstKind::Seq(nodes).into()
        }
    }

    fn trap() -> Self {
        AstKind::Panic.into()
    }

    fn statement(expr: &Self::Stmt) -> Self {
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
            Flow::Continue(shape) => {
                let code = format!(
                    "{}continue {}{};",
                    label,
                    c.label_prefix,
                    shape.index()
                );
                AstKind::Node(code).into()
            }
            Flow::Break(shape) => {
                let code = format!(
                    "{}break {}{};",
                    label,
                    c.label_prefix,
                    shape.index()
                );
                AstKind::Node(code).into()
            }
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

    fn switches<'a, I: Iterator<Item = (CondType<&'a Self::Expr>, Self)>>(
        conditionals: I,
        default_branch: Option<Self>,
    ) -> Self
    where
        Self::Expr: 'a,
    {
        let c = C::config();
        let mut branches: Vec<_> = flag_first(conditionals)
            .map(|(is_first, (cond_type, code))| {
                let cond = match cond_type {
                    CondType::Case(expr) => expr.to_string(),
                    CondType::CaseLabel(block_id) => format!(
                        "{} === {}",
                        c.control_variable,
                        block_id.index()
                    ),
                };
                if is_first {
                    AstKind::If(cond, Box::new(code.kind))
                } else {
                    AstKind::ElseIf(cond, Box::new(code.kind))
                }
            })
            .collect();

        if let Some(default_branch) = default_branch {
            branches.push(AstKind::Else(Box::new(default_branch.kind)).into());
        }

        AstKind::Seq(branches).into()
    }
}

fn flag_first<I: Iterator>(iter: I) -> impl Iterator<Item = (bool, I::Item)> {
    iter.enumerate().map(|(i, x)| (i == 0, x))
}
