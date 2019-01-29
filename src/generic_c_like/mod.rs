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
                // Use Either for this to avoid allocation?
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
        let branches = flag_first(conditionals)
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
            .chain(default_branch.into_iter().map(|default_branch| {
                AstKind::Else(Box::new(default_branch.kind))
            }))
            .map(AstKind::into);

        Self::merge(branches)
    }
}

fn flag_first<I: Iterator>(iter: I) -> impl Iterator<Item = (bool, I::Item)> {
    iter.enumerate().map(|(i, x)| (i == 0, x))
}

// # Very BASIC control flows
//
// These tests operate on generated string representation of pseduo code.
// They will need to be updated if additional optimization has been added to
// Relooper, or CLikeAst struct default behavior changes.
//
// Note: since tests compare generated string "code" vs. expected ignoring white
// spaces, block content and branch condition strings should not include white
// spaces either.
#[cfg(test)]
mod test_basics {
    use std::iter::Peekable;

    use super::{AstKind, CLikeAst};
    use crate::{Relooper, StructuredAst};

    #[test]
    fn test_basics_simple_x3() {
        let mut relooper: Relooper<String> = Relooper::new();

        let a = relooper.add_block("__block__a".to_string());
        let b = relooper.add_block("__block__b".to_string());
        let c = relooper.add_block("__block__c".to_string());

        // a +---> b +--> c
        relooper.add_branch(a, b, None);
        relooper.add_branch(b, c, None);

        let ast: CLikeAst = relooper.render(a).expect("Did not get shape");

        let pseudo_code = ast.to_string();
        let expected = "
            __block__a
            __block__b
            __block__c
        ";

        assert!(
            code_equal(pseudo_code.trim().chars(), expected.trim().chars()),
            pseudo_code
        );
    }

    #[test]
    fn test_basics_single_loop() {
        let mut relooper: Relooper<String> = Relooper::new();

        let a = relooper.add_block("i = 0".to_string());
        let b = relooper.add_block("__block__b".to_string());
        let c = relooper.add_block("i = i + 1".to_string());
        let d = relooper.add_block("__block__d".to_string());

        //       +--------+
        //       |        |
        //       v        |
        // a +-> b +----->c
        //       +
        //       |
        //       v
        //       d
        relooper.add_branch(a, b, None);
        relooper.add_branch(b, c, None);
        relooper.add_branch(b, d, Some("i > 10".to_string()));
        relooper.add_branch(c, b, None);

        let ast: CLikeAst = relooper.render(a).expect("Did not get shape");

        let pseudo_code = ast.to_string();
        let expected = "
            i = 0
            L1: while (true) {
              __block__b
              if ( i > 10 ) {
                break L1;
              }
              i = i + 1
              continue L1;
            }
            __block__d
        ";

        assert!(
            code_equal(pseudo_code.trim().chars(), expected.trim().chars()),
            pseudo_code
        );
    }

    #[test]
    fn test_basics_if_else() {
        let mut relooper: Relooper<&'static str> = Relooper::new();

        let a = relooper.add_block("__block__a");
        let b = relooper.add_block("__block__b");
        let c = relooper.add_block("__block__c");

        relooper.add_branch(a, b, Some("true"));
        relooper.add_branch(a, c, None);

        let ast: CLikeAst = relooper.render(a).expect("Did not get shape");

        let pseudo_code = ast.to_string();
        let expected = "
            __block__a
            if ( true ) {
              __block__b
            }
            else {
              __block__c
            }
        ";

        assert!(
            code_equal(pseudo_code.trim().chars(), expected.trim().chars()),
            pseudo_code
        );
    }

    #[test]
    fn test_basics_if() {
        let mut relooper: Relooper<&'static str> = Relooper::new();

        let a = relooper.add_block("__block__a");
        let b = relooper.add_block("__block__b");
        let c = relooper.add_block("__block__c");

        // a+---------->c
        // |            ^
        // |            |
        // +----->b+----+
        relooper.add_branch(a, b, Some("true"));
        relooper.add_branch(a, c, None);
        relooper.add_branch(b, c, None);

        let ast: CLikeAst = relooper.render(a).expect("Did not get shape");

        println!("{:#?}", ast);

        let pseudo_code = ast.to_string();
        let expected = "
              __block__a
              if ( true ) {
                __block__b
              }
              __block__c
        ";

        assert!(
            code_equal(pseudo_code.trim().chars(), expected.trim().chars()),
            pseudo_code,
        );
    }

    // Generic c like AST is used for unit test purposes
    // generally, white spaces are insignificant in this
    // style of pseudo code, by comparing generated pseudo
    // code, we can ensure relooper produces rightly "shaped"
    // code.
    fn code_equal<A, B>(x: A, y: B) -> bool
    where
        A: IntoIterator<Item = char>,
        B: IntoIterator<Item = char>,
    {
        let mut x = x.into_iter().filter(|c| !c.is_whitespace());
        let mut y = y.into_iter().filter(|c| !c.is_whitespace());

        loop {
            let c1 = x.next();
            let c2 = y.next();
            match (c1, c2) {
                (Some(c1), Some(c2)) if c1 == c2 => continue,
                (None, None) => return true,
                _ => return false,
            }
        }
    }

}
