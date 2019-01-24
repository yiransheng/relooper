mod ast;

use std::collections::HashMap;
use std::hash::Hash;

use petgraph::graphmap::DiGraphMap;

use self::ast::{Code, Edge, Node, Vertex};
use crate::{CondType, Exit, Flow, ShapeId, StructuredAst};

impl StructuredAst for Code<i32, i32> {
    type Stmt = i32;
    type Expr = i32;

    fn trap() -> Self {
        Code::singleton(Node::Panic)
    }
    fn nop() -> Self {
        Code::singleton(Node::Nop)
    }
    fn statement(stmt: &Self::Stmt) -> Self {
        Code::singleton(Node::Opaque(*stmt))
    }

    fn exit(b: Exit) -> Self {
        let node1 = b.set_label.map(Node::SetLabel);
        let node2 = match b.flow {
            Flow::Direct => Node::Nop,
            Flow::Break(shape_id) => Node::Break(shape_id),
            Flow::Continue(shape_id) => Node::Continue(shape_id),
        };
        if let Some(node1) = node1 {
            Code {
                list: vector![node1, node2],
            }
        } else {
            Code::singleton(node2)
        }
    }

    fn join(mut self, other: Self) -> Self {
        self.list.append(other.list);
        self
    }

    fn wrap_in_loop(self, shape_id: ShapeId) -> Self {
        let node = Node::Loop(shape_id, self);
        Code::singleton(node)
    }

    fn wrap_in_block(self, shape_id: ShapeId) -> Self {
        let node = Node::Block(shape_id, self);
        Code::singleton(node)
    }

    fn switches<'a, I: Iterator<Item = (CondType<&'a Self::Expr>, Self)>>(
        conditionals: I,
        default_branch: Option<Self>,
    ) -> Self {
        let branches: HashMap<CondType<i32>, _> = conditionals
            .map(|(cond, code)| (cond.as_owned(), code))
            .collect();
        let node = Node::Switches(branches, default_branch);
        Code::singleton(node)
    }
}

#[cfg(test)]
mod test_breaks_iter {
    use super::*;
    use crate::{Flow, Relooper};

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
        relooper.add_branch(c, d, Some(99));

        let ast: Code<i32, i32> =
            relooper.render(a).expect("Did not get shape");

        println!("{:#?}", ast);
        let mut graph: DiGraphMap<_, _> = DiGraphMap::default();
        ast.build_graph(Vertex::Entry, &mut graph);

        println!("{:#?}", graph);

        assert!(false);
    }
}
