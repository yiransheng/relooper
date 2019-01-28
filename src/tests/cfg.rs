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
    discovered: HashSet<NodeState<L, C>>,
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
            discovered: HashSet::default(),
            queue,
        }
    }

    pub fn next(
        &mut self,
        graph: &DiGraphMap<Node<L>, Edge<C>>,
    ) -> Option<(L, Option<C>)> {
        while let Some((src, label, cond)) = self.queue.pop_front() {
            let mut ret = None;

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

                if !self.discovered.contains(&node_state) {
                    let cond = node_state.2.clone();
                    let enque_state = node_state.clone();

                    self.discovered.insert(node_state);

                    match node {
                        Node::Meaningful(node) => {
                            ret = Some((node, cond));
                        }
                        _ => {
                            self.queue.push_back(enque_state);
                        }
                    }
                }
            }

            if ret.is_some() {
                return ret;
            }
        }

        None
    }
}
