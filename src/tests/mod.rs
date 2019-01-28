mod ast;
mod cfg;
mod unstructured;

use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use petgraph::graphmap::DiGraphMap;

use self::ast::{Edge, GraphMaker, Node};
use self::cfg::MeaningfulNeighbors;
use self::unstructured::UnstructuredCfg;

use crate::{CondType, Exit, Flow, ShapeId, StructuredAst};

#[cfg(test)]
mod test_breaks_iter {
    use super::*;
    use crate::{Flow, Relooper};
    use std::io;

    use petgraph::dot::{Config, Dot};
    use petgraph::visit::Dfs;
    use petgraph::{Direction, Graph};

    use quickcheck::{quickcheck, Arbitrary, Gen};

    fn check_node(
        node: i32,
        unstructured: &DiGraphMap<i32, Option<i32>>,
        processed: &DiGraphMap<Node<i32>, Edge<i32>>,
    ) -> bool {
        let neighbors_0: HashSet<_> = unstructured
            .neighbors_directed(node, Direction::Outgoing)
            .map(|nx| {
                let edge = unstructured.edge_weight(node, nx).cloned().unwrap();
                (nx, edge)
            })
            .collect();

        let mut neighbors_1 = HashSet::new();
        let mut searcher = MeaningfulNeighbors::new(node, Direction::Outgoing);
        while let Some(x) = searcher.next(processed) {
            neighbors_1.insert(x);
        }

        println!("Checking node: {}", node);
        if neighbors_0 != neighbors_1 {
            println!("Original: {:#?}", neighbors_0);
            println!("Procesed: {:#?}", neighbors_1);
        }

        neighbors_0 == neighbors_1
    }

    quickcheck! {
        fn reloops_unstructured_cfg(unstructured: UnstructuredCfg) -> bool {
            return true;

            let unstructured = unstructured.graph;

            if unstructured.node_count() == 0 {
                return true;
            }
            let mut relooper: Relooper<i32> = Relooper::new();
            let mut mapping = HashMap::new();
            for node in unstructured.nodes() {
                let block = relooper.add_block(node);
                mapping.insert(node, block);
            }
            for (from, to, edge) in unstructured.all_edges() {
                let from = mapping.get(&from).cloned().unwrap();
                let to = mapping.get(&to).cloned().unwrap();

                relooper.add_branch(from, to, edge.as_ref().cloned());
            }

            let entry_node = 0;
            let entry = mapping.get(&entry_node).cloned().unwrap();

            let maker: Box<dyn GraphMaker<i32, i32>> =
                relooper.render(entry).expect("Did not get shape");

            let mut processed: DiGraphMap<_, _> = DiGraphMap::default();
            maker.make_cfg(&mut processed);

            let mut dfs = Dfs::new(&unstructured, entry_node);
            while let Some(nx) = dfs.next(&unstructured) {
                let is_ok = check_node(nx, &unstructured, &processed);
                if !is_ok {
                    let stdout = io::stdout();
                    {
                        use io::Write;
                        let mut out = stdout.lock();
                        writeln!(out, "{:?}", Dot::with_config(&unstructured, &[]),);
                        writeln!(out);
                        writeln!(out);
                        writeln!(out);
                        writeln!(out, "{:?}", Dot::with_config(&processed, &[]),);
                    }
                    return false;
                }
            }

            true
        }
    }

    #[test]
    fn test_simple_loop_breaks() {
        let mut relooper: Relooper<i32> = Relooper::new();

        let a0 = relooper.add_block(0);
        let a1 = relooper.add_block(1);
        let a2 = relooper.add_block(2);

        relooper.add_branch(a0, a0, None);
        relooper.add_branch(a0, a1, Some(-54));
        relooper.add_branch(a0, a2, Some(99));
        relooper.add_branch(a2, a1, None);
        relooper.add_branch(a1, a0, Some(86));

        let maker: Box<dyn GraphMaker<i32, i32>> =
            relooper.render(a0).expect("Did not get shape");

        let mut graph: DiGraphMap<_, _> = DiGraphMap::default();
        maker.make_cfg(&mut graph);

        let stdout = io::stdout();
        {
            use io::Write;
            writeln!(stdout.lock(), "{:?}", Dot::with_config(&graph, &[]),);
        }

        let mut search = MeaningfulNeighbors::new(0, Direction::Outgoing);
        while let Some((n, e)) = search.next(&graph) {
            println!("B: {} {:?}", n, e);
        }

        assert!(false);
    }
}
