use petgraph::graphmap::DiGraphMap;
use petgraph::{Direction, Graph};
use quickcheck::{Arbitrary, Gen};

#[derive(Debug, Clone)]
pub struct UnstructuredCfg {
    pub graph: DiGraphMap<i32, Option<i32>>,
}

impl Arbitrary for UnstructuredCfg {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        // let node_count = usize::arbitrary(g) % 8;
        let node_count = 3;
        let max_edge_count = usize::arbitrary(g) % 4 + 1;

        let graph = random_unstructured(node_count, max_edge_count, g);

        UnstructuredCfg { graph }
    }
}

fn random_unstructured<G: Gen>(
    node_count: usize,
    max_edge_count: usize,
    g: &mut G,
) -> DiGraphMap<i32, Option<i32>> {
    let mut graph = DiGraphMap::new();

    if node_count == 0 {
        return graph;
    }

    for node in 0..node_count {
        let node = node as i32;
        graph.add_node(node);

        // each node has a single default target
        let default_target = usize::arbitrary(g) % node_count;
        graph.add_node(default_target as i32);
        graph.add_edge(node, default_target as i32, None);

        let edge_count = usize::arbitrary(g) % max_edge_count;
        for _ in 0..edge_count {
            let target = usize::arbitrary(g) % node_count;
            let target = target as i32;
            graph.add_node(target);

            // no existing branch
            if graph.edge_weight(node, target).is_none() {
                let weight = i32::arbitrary(g);
                graph.add_edge(node, target, Some(weight));
            }
        }
    }

    graph
}
