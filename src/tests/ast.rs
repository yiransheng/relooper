use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeMap, HashMap};
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::iter;
use std::marker::PhantomData;

use petgraph::graphmap::DiGraphMap;

use im::vector::{Iter, Vector};

use crate::{BlockId, CondType, Exit, Flow, ShapeId, StructuredAst};

pub trait GraphMaker<L, C> {
    fn make_cfg(
        &self,
        graph: &mut DiGraphMap<Node<L>, Edge<C>>,
    ) -> (Node<L>, Option<Node<L>>)
    where
        L: Copy + Hash + Eq + Ord;
}
impl<L, C> GraphMaker<L, C> for Box<dyn GraphMaker<L, C>> {
    fn make_cfg(
        &self,
        graph: &mut DiGraphMap<Node<L>, Edge<C>>,
    ) -> (Node<L>, Option<Node<L>>)
    where
        L: Copy + Hash + Eq + Ord,
    {
        (**self).make_cfg(graph)
    }
}

impl<L, C> StructuredAst for Box<dyn GraphMaker<L, C>>
where
    C: Eq + Hash + Clone + 'static,
    L: Copy + Hash + Eq + Ord + 'static,
{
    type Expr = C;
    type Stmt = L;

    fn merge<I>(mut nodes: I) -> Self
    where
        Self: Sized,
        I: Iterator<Item = Self>,
    {
        merge_makers(nodes)
    }

    fn trap() -> Self {
        panic!()
    }

    fn statement(stmt: &Self::Stmt) -> Self {
        println!("Stmt");
        Box::new(OriginalNode { node: *stmt })
    }

    fn exit(b: Exit) -> Self {
        Box::new(b)
    }

    fn wrap_in_loop(self, shape_id: ShapeId) -> Self {
        self.wrap_in_block(shape_id)
    }

    fn wrap_in_block(self, shape_id: ShapeId) -> Self {
        let block = Block {
            id: shape_id,
            inner: self,
        };
        Box::new(block)
    }

    fn switches<'a, I: Iterator<Item = (CondType<&'a Self::Expr>, Self)>>(
        conditionals: I,
        default_branch: Option<Self>,
    ) -> Self
    where
        Self::Expr: 'a,
    {
        let branches = conditionals
            .map(|(cond, inner)| (cond.cloned(), inner))
            .collect();

        let branching = Branching {
            branches,
            default_branch,
        };

        Box::new(branching)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Node<L> {
    Original(L),
    ShapeEnter(ShapeId),
    ShapeExit(ShapeId),
    Dummy(usize, Option<BlockId>),
}

impl<L> Node<L> {
    fn make_dummy<C>(
        graph: &mut DiGraphMap<Node<L>, Edge<C>>,
        label: Option<BlockId>,
    ) -> Self
    where
        L: Copy + Hash + Eq + Ord,
    {
        let node = Node::Dummy(graph.node_count(), label);
        graph.add_node(node);
        node
    }
}

struct OriginalNode<L> {
    node: L,
}

impl<L, C> GraphMaker<L, C> for OriginalNode<L> {
    fn make_cfg(
        &self,
        graph: &mut DiGraphMap<Node<L>, Edge<C>>,
    ) -> (Node<L>, Option<Node<L>>)
    where
        L: Copy + Hash + Eq + Ord,
    {
        let node = Node::Original(self.node);

        graph.add_node(node);

        (node, Some(node))
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Edge<C> {
    Forward,
    Conditional(C),
    MatchLabel(BlockId),
    Backward,
}

impl<L, C> GraphMaker<L, C> for Exit {
    fn make_cfg(
        &self,
        graph: &mut DiGraphMap<Node<L>, Edge<C>>,
    ) -> (Node<L>, Option<Node<L>>)
    where
        L: Copy + Hash + Eq + Ord,
    {
        let node = Node::make_dummy(graph, self.set_label);

        match self.flow {
            Flow::Direct => (node, Some(node)),
            Flow::Break(shape) => {
                let shape = Node::ShapeExit(shape);
                graph.add_node(shape);
                graph.add_edge(node, shape, Edge::Forward);

                (node, None)
            }
            Flow::Continue(shape) => {
                let shape = Node::ShapeEnter(shape);
                graph.add_node(shape);
                graph.add_edge(node, shape, Edge::Backward);

                (node, None)
            }
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct Nop;

impl<L, C> GraphMaker<L, C> for Nop {
    fn make_cfg(
        &self,
        graph: &mut DiGraphMap<Node<L>, Edge<C>>,
    ) -> (Node<L>, Option<Node<L>>)
    where
        L: Copy + Hash + Eq + Ord,
    {
        let node = Node::make_dummy(graph, None);

        (node, Some(node))
    }
}

struct Block<G> {
    id: ShapeId,
    inner: G,
}

impl<L, C, G> GraphMaker<L, C> for Block<G>
where
    G: GraphMaker<L, C>,
{
    fn make_cfg(
        &self,
        graph: &mut DiGraphMap<Node<L>, Edge<C>>,
    ) -> (Node<L>, Option<Node<L>>)
    where
        L: Copy + Hash + Eq + Ord,
    {
        let entry_node = Node::ShapeEnter(self.id);
        let exit_node = Node::ShapeExit(self.id);

        graph.add_node(entry_node);
        graph.add_node(exit_node);

        let (inner_node, inner_exit) = self.inner.make_cfg(graph);

        graph.add_edge(entry_node, inner_node, Edge::Forward);

        if let Some(inner_exit) = inner_exit {
            if exit_node != inner_exit {
                graph.add_edge(inner_exit, exit_node, Edge::Forward);
            }
        }

        (entry_node, Some(exit_node))
    }
}

struct Branching<C, G> {
    branches: HashMap<CondType<C>, G>,
    default_branch: Option<G>,
}

impl<L, C, G> GraphMaker<L, C> for Branching<C, G>
where
    G: GraphMaker<L, C>,
    C: Eq + Hash + Clone,
{
    fn make_cfg(
        &self,
        graph: &mut DiGraphMap<Node<L>, Edge<C>>,
    ) -> (Node<L>, Option<Node<L>>)
    where
        L: Copy + Hash + Eq + Ord,
    {
        let entry = Node::make_dummy(graph, None);
        let exit = Node::make_dummy(graph, None);

        for (inner, edge) in self
            .branches
            .iter()
            .map(|(cond, inner)| match cond {
                CondType::Case(cond) => {
                    (inner, Edge::Conditional(cond.clone()))
                }
                CondType::CaseLabel(label) => (inner, Edge::MatchLabel(*label)),
            })
            .chain(
                self.default_branch
                    .as_ref()
                    .into_iter()
                    .map(|inner| (inner, Edge::Forward)),
            )
        {
            let (to_node, inner_exit) = inner.make_cfg(graph);

            graph.add_edge(entry, to_node, edge);
            if let Some(inner_exit) = inner_exit {
                graph.add_edge(inner_exit, exit, Edge::Forward);
            }
        }

        (entry, Some(exit))
    }
}

struct Chain<L, C, G> {
    inner: G,
    next: Option<Box<dyn GraphMaker<L, C>>>,
}

fn merge_makers<L, C, I>(mut xs: I) -> Box<dyn GraphMaker<L, C>>
where
    I: Iterator<Item = Box<dyn GraphMaker<L, C>>>,
    C: Eq + Hash + Clone,
    L: Copy + Hash + Eq + Ord,
    L: 'static,
    C: 'static,
{
    let head = xs.next();

    if let Some(head) = head {
        let chained = Chain {
            inner: head,
            next: Some(merge_makers(xs)),
        };
        Box::new(chained)
    } else {
        Box::new(Nop)
    }
}

impl<L, C, G> GraphMaker<L, C> for Chain<L, C, G>
where
    G: GraphMaker<L, C>,
    C: Eq + Hash + Clone,
{
    fn make_cfg(
        &self,
        graph: &mut DiGraphMap<Node<L>, Edge<C>>,
    ) -> (Node<L>, Option<Node<L>>)
    where
        L: Copy + Hash + Eq + Ord,
    {
        let (entry, exit) = self.inner.make_cfg(graph);

        if let Some(next) = self.next.as_ref() {
            let (next_entry, next_exit) = next.make_cfg(graph);
            if let Some(exit) = exit {
                graph.add_edge(exit, next_entry, Edge::Forward);
            }

            (entry, next_exit)
        } else {
            (entry, exit)
        }
    }
}
