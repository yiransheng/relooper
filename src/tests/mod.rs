mod ast;

use std::collections::HashMap;
use std::hash::Hash;

use petgraph::graphmap::DiGraphMap;

use self::ast::{Edge, GraphMaker, Node};
use crate::{CondType, Exit, Flow, ShapeId, StructuredAst};

#[cfg(test)]
mod test_breaks_iter {
    use super::*;
    use crate::{Flow, Relooper};
    use std::io;

    use petgraph::dot::{Config, Dot};
    use petgraph::Graph;

    #[test]
    fn test_simple_loop_breaks() {
        let mut relooper: Relooper<i32> = Relooper::new();

        let a = relooper.add_block(0);
        let b = relooper.add_block(1);
        let c = relooper.add_block(2);
        let d = relooper.add_block(3);

        //       +--------+
        //       |        |
        //       v        |
        // a +-> b +----->c
        //       +        +
        //       |        |
        //       v        |
        //       d <------+
        relooper.add_branch(a, b, None);
        relooper.add_branch(b, c, None);
        relooper.add_branch(b, d, Some(-1));
        relooper.add_branch(c, b, None);
        // relooper.add_branch(c, d, Some(99));

        let maker: Box<dyn GraphMaker<i32, i32>> =
            relooper.render(a).expect("Did not get shape");

        let mut graph: DiGraphMap<_, _> = DiGraphMap::default();
        maker.make_cfg(&mut graph);

        let stdout = io::stdout();
        {
            use io::Write;
            writeln!(stdout.lock(), "{:?}", Dot::with_config(&graph, &[]),);
        }

        assert!(false);
    }
}
