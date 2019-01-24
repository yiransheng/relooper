use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

use petgraph::graphmap::DiGraphMap;

use im::vector::{Iter, Vector};

use crate::{BlockId, CondType, Exit, Flow, ShapeId, StructuredAst};

#[derive(Debug, Clone)]
pub struct Code<L: Clone, C: Eq + Hash + Clone> {
    pub list: Vector<Node<L, C>>,
}

impl<L: Clone, C: Eq + Hash + Clone> Code<L, C> {
    pub fn singleton(node: Node<L, C>) -> Self {
        Code {
            list: Vector::unit(node),
        }
    }

    pub fn nodes(&self) -> Iter<Node<L, C>> {
        self.list.iter()
    }
}

#[derive(Debug, Clone)]
pub enum Node<L: Clone, C: Eq + Hash + Clone> {
    Nop,
    Panic,
    Opaque(L),
    SetLabel(BlockId),
    Break(ShapeId),
    Continue(ShapeId),
    Switches(HashMap<CondType<C>, Code<L, C>>, Option<Code<L, C>>),
    Loop(ShapeId, Code<L, C>),
    Block(ShapeId, Code<L, C>),
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Vertex<L> {
    Entry,
    Original(L),
    SetLabel(BlockId),
    Shape(ShapeId),
}

#[derive(Debug)]
pub enum Edge<C> {
    Direct,
    Condition(CondType<C>),
    Otherwise,
    Continue,
    Break,
}

impl<L: Debug + Copy + Hash + Eq + Ord, C: Debug + Eq + Hash + Clone>
    Code<L, C>
{
    pub fn build_graph(
        &self,
        entry: Vertex<L>,
        graph: &mut DiGraphMap<Vertex<L>, Edge<C>>,
    ) {
        let mut vertex = entry;

        for node in self.nodes() {
            match node {
                Node::Opaque(node) => {
                    let v = Vertex::Original(node.clone());
                    graph.add_node(v);
                    graph.add_edge(vertex, v, Edge::Direct);
                    vertex = v;
                }
                Node::Nop => continue,
                Node::Panic => break,
                Node::SetLabel(label) => {
                    let v = Vertex::SetLabel(*label);
                    graph.add_node(v);

                    graph.add_edge(vertex, v, Edge::Direct);
                    vertex = v;
                }
                Node::Break(shape_id) => {
                    let v = Vertex::Shape(*shape_id);
                    graph.add_edge(vertex, v, Edge::Break);
                }
                Node::Continue(shape_id) => {
                    let v = Vertex::Shape(*shape_id);
                    graph.add_edge(vertex, v, Edge::Continue);
                }
                Node::Switches(branches, default_branch) => {
                    for (edge, code) in branches
                        .iter()
                        .map(|(cond, code)| {
                            (Edge::Condition(cond.clone()), code)
                        })
                        .chain(
                            default_branch
                                .as_ref()
                                .map(|code| (Edge::Otherwise, code))
                                .into_iter(),
                        )
                    {
                        code.build_graph(vertex, graph);

                        graph.add_edge(vertex, entry, edge);
                    }
                }
                Node::Loop(shape_id, code) => {
                    let v = Vertex::Shape(*shape_id);
                    graph.add_edge(vertex, v, Edge::Direct);

                    vertex = v;

                    code.build_graph(vertex, graph);
                }
                Node::Block(shape_id, code) => {
                    let v = Vertex::Shape(*shape_id);
                    graph.add_edge(vertex, v, Edge::Direct);

                    vertex = v;

                    code.build_graph(vertex, graph);
                }
            }
        }
    }
}
