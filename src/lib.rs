mod process;
mod relooper;
mod render;
mod shapes;
mod types;

pub use crate::relooper::Relooper;
pub use crate::render::{CondType, Exit, Flow, StructedAst};
pub use crate::types::{BlockId, ShapeId};

pub mod generic_c_like;

#[cfg(test)]
mod tests {
    use super::Relooper;
    use crate::generic_c_like::{AstKind, CLikeAst};

    #[test]
    fn test_basics_simple_x3() {
        let mut relooper: Relooper<String> = Relooper::new();

        let a = relooper.add_block("// block a".to_string());
        let b = relooper.add_block("// block b".to_string());
        let c = relooper.add_block("// block c".to_string());

        // FIXME
        relooper.add_branch(a, b, Some("a -> b".to_string()));
        relooper.add_branch(b, c, Some("b -> c".to_string()));

        let ast: CLikeAst = relooper.render(a).expect("Did not get shape");

        let expected_ast = AstKind::Seq(vec![
            AstKind::Node("// block a".to_string()),
            AstKind::Node("// block b".to_string()),
            AstKind::Node("// block c".to_string()),
        ])
        .into();

        assert_eq!(ast, expected_ast);
    }

    #[test]
    fn test_basics_single_loop() {
        let mut relooper: Relooper<String> = Relooper::new();

        let a = relooper.add_block("i = 0".to_string());
        let b = relooper.add_block("// block b".to_string());
        let c = relooper.add_block("i = i + 1".to_string());
        let d = relooper.add_block("// block d".to_string());

        relooper.add_branch(a, b, None);
        relooper.add_branch(b, c, None);
        relooper.add_branch(b, d, Some("i > 10".to_string()));
        relooper.add_branch(c, b, None);

        let ast: CLikeAst = relooper.render(a).expect("Did not get shape");

        println!("{}", ast);

        let expected_ast = AstKind::Seq(vec![
            AstKind::Node("// block a".to_string()),
            AstKind::Node("// block b".to_string()),
            AstKind::Node("// block c".to_string()),
        ])
        .into();

        assert_eq!(ast, expected_ast);
    }
}
