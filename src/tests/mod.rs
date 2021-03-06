mod meaningful_neighbors;
mod structured;
mod unstructured;

#[cfg(test)]
mod quickcheck_graphs {
    use std::collections::{HashMap, HashSet};

    use crate::*;

    use super::meaningful_neighbors::MeaningfulNeighbors;
    use super::structured::{Edge, GraphMaker, Node};
    use super::unstructured::UnstructuredCfg;

    use petgraph::dot::{Config, Dot};
    use petgraph::graphmap::DiGraphMap;
    use petgraph::visit::Dfs;
    use petgraph::{Direction, Graph};

    use quickcheck::{quickcheck, Arbitrary, Gen};

    quickcheck! {
        fn reloops_unstructured_cfg(unstructured: UnstructuredCfg) -> bool {
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
                    print_graphs(&unstructured, &processed);
                    return false;
                }
            }

            true
        }
    }

    fn print_graphs(
        unstructured: &DiGraphMap<i32, Option<i32>>,
        processed: &DiGraphMap<Node<i32>, Edge<i32>>,
    ) {
        use std::io::{self, Write};
        let stdout = io::stdout();
        let mut out = stdout.lock();

        writeln!(out, "{:?}", Dot::with_config(&unstructured, &[]),);
        writeln!(out);
        writeln!(out);
        writeln!(out);
        writeln!(out, "{:?}", Dot::with_config(&processed, &[]),);
    }

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

        if neighbors_0 != neighbors_1 {
            println!("Checking node: {}", node);
            println!("Original: {:#?}", neighbors_0);
            println!("Procesed: {:#?}", neighbors_1);
        }

        neighbors_0 == neighbors_1
    }
}
