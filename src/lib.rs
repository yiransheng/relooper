mod shapes;
mod types;

use std::marker::PhantomData;
use std::ops::Index;

use fixedbitset::FixedBitSet;
use petgraph::graph::{DiGraph, EdgeIndex, IndexType, NodeIndex};

use types::*;

struct CFGSubset<'a, L, B> {
    blocks: &'a mut BlockSet,
    entries: &'a BlockSet,
    cfgraph: &'a mut CFGraph<L, B>,
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
