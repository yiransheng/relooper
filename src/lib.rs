mod process;
mod shapes;
mod types;
mod utils;

use std::marker::PhantomData;
use std::ops::Index;

use fixedbitset::FixedBitSet;
use petgraph::graph::{DiGraph, EdgeIndex, IndexType, NodeIndex};

use types::*;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
