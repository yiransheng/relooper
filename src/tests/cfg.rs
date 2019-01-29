use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Debug;
use std::hash::Hash;

use super::ast::{Edge, Node};

use crate::types::{BlockId, ShapeId};
use petgraph::graphmap::DiGraphMap;
use petgraph::Direction;

type NodeState<L, C> = (Node<L>, Option<BlockId>, Option<C>);

pub struct MeaningfulNeighbors<L, C> {
    dir: Direction,
    src: Node<L>,
    is_initial: bool,
    discovered: HashSet<NodeState<L, C>>,
    branches_taken: HashSet<Option<C>>,
    queue: VecDeque<NodeState<L, C>>,
}
impl<L, C> MeaningfulNeighbors<L, C>
where
    C: Eq + Hash + Clone + 'static,
    L: Copy + Hash + Eq + Ord + 'static,
{
    pub fn new(node: L, dir: Direction) -> Self {
        let mut queue = VecDeque::new();
        queue.push_back((Node::Meaningful(node), None, None));

        MeaningfulNeighbors {
            dir,
            src: Node::Meaningful(node),
            is_initial: true,
            discovered: HashSet::default(),
            branches_taken: HashSet::default(),
            queue,
        }
    }

    pub fn next(
        &mut self,
        graph: &DiGraphMap<Node<L>, Edge<C>>,
    ) -> Option<(L, Option<C>)> {
        while let Some((src, label, cond)) = self.queue.pop_front() {
            if let Node::Meaningful(x) = src {
                if src == self.src && self.is_initial {
                    self.is_initial = false;
                } else {
                    if self.branches_taken.contains(&cond) {
                        continue;
                    } else {
                        self.branches_taken.insert(cond.clone());
                        return Some((x, cond));
                    }
                }
            }

            if self.discovered.contains(&(src, label, cond.clone())) {
                continue;
            } else {
                self.discovered.insert((src, label, cond.clone()));
            }

            for node in graph.neighbors_directed(src, self.dir) {
                let edge = graph.edge_weight(src, node).unwrap();

                if let Edge::MatchLabel(match_label) = edge {
                    if label != Some(*match_label) {
                        continue;
                    }
                }

                let mut node_state = match node {
                    Node::Dummy(_, Some(label)) => {
                        (node, Some(label), cond.clone())
                    }
                    _ => (node, label, cond.clone()),
                };

                match edge {
                    Edge::Conditional(cond) => {
                        node_state.2 = Some(cond.clone());
                    }
                    _ => {}
                }

                if node_state.1.is_some() && node_state.2.is_some() {}

                self.queue.push_back(node_state);
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    use maplit::hashset;

    #[test]
    fn test_multi_direct_neighbors() {
        let mut graph: DiGraphMap<Node<i32>, Edge<i32>> =
            DiGraphMap::from_edges(&[
                (Node::Meaningful(0), Node::Dummy(0, None), Edge::Forward),
                (
                    Node::Dummy(0, None),
                    Node::Meaningful(1),
                    Edge::Conditional(-1),
                ),
                (
                    Node::Dummy(0, None),
                    Node::Meaningful(2),
                    Edge::Conditional(-2),
                ),
                (Node::Dummy(0, None), Node::Meaningful(3), Edge::Forward),
            ]);

        let mut searcher = MeaningfulNeighbors::new(0, Direction::Outgoing);
        let mut neighbors = HashSet::new();

        while let Some(x) = searcher.next(&graph) {
            neighbors.insert(x);
        }

        let expected = hashset! {
            (1i32, Some(-1i32)),
            (2, Some(-2)),
            (3, None)
        };

        assert_eq!(&expected, &neighbors);
    }
}
