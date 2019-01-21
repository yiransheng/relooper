// use petgraph::Direction;

use crate::process::{process, CFGSubset, GraphEnv};
use crate::render::StructedAst;
use crate::shapes::*;
use crate::types::*;

pub struct Relooper<L, C = L> {
    cfgraph: CFGraph<L, C>,
}

impl<E> Relooper<E, E> {
    pub fn render<S: StructedAst>(self, entry: BlockId) -> Option<S>
    where
        E: AsRef<S::Expr>, // E: ::std::fmt::Debug
    {
        let shape = self.calculate(entry);

        shape.map(|mut shape| {
            shape.fuse();
            shape.render(&shape)
        })
    }
}

impl<L, C> Relooper<L, C> {
    pub fn new() -> Self {
        Relooper {
            cfgraph: CFGraph::default(),
        }
    }
    pub fn add_block(&mut self, block: L) -> BlockId {
        self.cfgraph.add_node(Block::Raw(block))
    }
    pub fn add_branch(&mut self, a: BlockId, b: BlockId, cond: Option<C>) {
        // let default_branch_exists = self
        // .cfgraph
        // .neighbors_directed(a, Direction::Outgoing)
        // .filter_map(|t| self.cfgraph.find_edge(a, t))
        // .filter_map(|e| self.cfgraph.edge_weight(e))
        // .any(|b| {
        // if let Branch::Raw(None) = b {
        // true
        // } else {
        // false
        // }
        // });

        self.cfgraph.add_edge(a, b, Branch::Raw(cond));
    }

    fn calculate(self, entry: BlockId) -> Option<Shape<L, C>> {
        let mut env = GraphEnv::new(self.cfgraph);
        let mut blocks = env.remove_dead(entry);

        let mut entries = env.empty_block_set();
        let mut next_entries = env.empty_block_set();

        entries.insert(entry);

        // return None?
        assert!(!blocks.is_empty(), "Nothing is reachable");

        let subset =
            CFGSubset::new(&mut entries, &mut next_entries, &mut blocks);
        process(subset, &mut env)
    }
}

impl<L, C> Default for Relooper<L, C> {
    fn default() -> Self {
        Self::new()
    }
}
