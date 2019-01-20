use std::collections::{HashMap, VecDeque};
use std::ops::{Deref, DerefMut};

use petgraph::Direction;
use smallvec::SmallVec;

use crate::shapes::*;
use crate::types::*;

type SVec<T> = SmallVec<[T; 4]>;

type IndepSetMap = HashMap<BlockId, BlockSet>;

#[derive(Debug)]
pub struct CFGSubset<'a> {
    blocks: &'a mut BlockSet,
    entries: &'a mut BlockSet,
    next_entries: &'a mut BlockSet,
}

pub struct GraphEnv<L, C> {
    shape_id_gen: ShapeIdGen,
    cfgraph: CFGraph<L, C>,
}
impl<L, C> GraphEnv<L, C> {
    pub fn new(cfgraph: CFGraph<L, C>) -> Self {
        GraphEnv {
            shape_id_gen: ShapeIdGen::default(),
            cfgraph,
        }
    }
    pub fn remove_dead(&mut self, entry: BlockId) {
        use petgraph::visit::Dfs;
        let mut dead = self.all_block_set();
        let mut dfs = Dfs::new(&self.cfgraph, entry);

        while let Some(node_id) = dfs.next(&self.cfgraph) {
            dead.remove(node_id);
        }

        for node_id in dead.iter() {
            self.cfgraph.remove_node(node_id);
        }
    }
    pub fn all_block_set(&self) -> BlockSet {
        let mut set = self.empty_block_set();
        set.extend(self.cfgraph.node_indices());
        set
    }
    pub fn empty_block_set(&self) -> BlockSet {
        BlockSet::new_empty(self.cfgraph.node_count())
    }
}
impl<L, C> Deref for GraphEnv<L, C> {
    type Target = CFGraph<L, C>;

    fn deref(&self) -> &Self::Target {
        &self.cfgraph
    }
}
impl<L, C> DerefMut for GraphEnv<L, C> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.cfgraph
    }
}

fn empty_block_set<L, C>(g: &CFGraph<L, C>) -> BlockSet {
    BlockSet::new_empty(g.node_count())
}

pub fn process<'a, L, C>(
    mut subset: CFGSubset<'a>,
    env: &mut GraphEnv<L, C>,
) -> Option<Shape<L, C>> {
    let mut shape: Option<Shape<L, C>> = None;
    let mut has_ret = false;
    let mut prev: &mut Link<L, C> = &mut None;
    let mut multi_entry_type: EntryType = EntryType::Checked;

    // this macro (and the general shape of process function) is more or less a
    // direct translate of Emscript relooper C++ implementation there are probably
    // more ideomatic rust ways of doing things, but for now at least, keep it
    // close to the official version
    macro_rules! make {
        ($call: expr) => {{
            let (s, next_subset) = $call?;

            if let ShapeKind::Simple(_) = &s.kind {
                multi_entry_type = EntryType::Direct;
            } else {
                multi_entry_type = EntryType::Checked;
            }

            if !has_ret {
                has_ret = true;
                shape = Some(s);
                prev = (&mut shape).as_mut().map(|s| &mut s.next).unwrap();
            } else {
                *prev = Some(Box::new(s));
                prev = prev.as_mut().map(|s| &mut s.next).unwrap();
            }

            if next_subset.entries.is_empty() {
                return shape;
            } else {
                subset = next_subset;
                continue;
            }
        }};
    }

    loop {
        if subset.entries.is_empty() {
            return shape;
        }
        if subset.entries.len() == 1 {
            let single_entry = subset.entries.sample_one().unwrap();
            // Case 1: no branch returns to entry
            if env.blocks_in(single_entry, &*subset.blocks).count() == 0 {
                make!(subset.make_simple(env));
            }

            // Case 2: some branch(es) returns to entry
            make!(subset.make_loop(env));
        }

        let mut indep_groups = subset.find_independent_groups(env);

        let has_multi_entry = indep_groups.values().any(|g| !g.is_empty());

        // Case 3: multiple entry some entry has non-empty indepent group
        if has_multi_entry {
            make!(subset.make_multiple(
                multi_entry_type,
                &mut indep_groups,
                env
            ));
        }
        // Case 4: no entry has indepent group
        make!(subset.make_loop(env));
    }
}

impl<'a> CFGSubset<'a> {
    pub fn new(
        next_entries: &'a mut BlockSet,
        prev_entries: &'a mut BlockSet,
        blocks: &'a mut BlockSet,
    ) -> Self {
        prev_entries.clear();
        CFGSubset {
            blocks,
            entries: next_entries,
            next_entries: prev_entries,
        }
    }

    fn make_simple<L, C>(
        self,
        env: &mut GraphEnv<L, C>,
    ) -> Option<(Shape<L, C>, Self)> {
        let CFGSubset {
            blocks,
            entries,
            next_entries,
        } = self;

        // generate new shape id
        let shape_id: ShapeId = env.next_shape_id();

        // single entry (entries.len() == 1 checked in caller)
        let internal_id: BlockId = entries.sample_one()?;

        // populate next entries
        next_entries.extend(env.blocks_out(internal_id, blocks));

        let mut targets: SVec<BlockId> = env
            .neighbors_directed(internal_id, Direction::Outgoing)
            .collect();

        // solipsize/process branches
        for t in targets.iter().cloned() {
            if let Some(branch) = env.find_branch_mut(internal_id, t) {
                branch.solipsize(t, FlowType::Direct, shape_id);
            }
        }

        // take processed branches (leaving dummy Branch::Registered in graph)
        let shape_branches_out: HashMap<_, _> = targets
            .drain()
            .filter_map(|t| {
                if let Some(branch) = env.find_branch_mut(internal_id, t) {
                    branch.take().map(|b| (t, b))
                } else {
                    None
                }
            })
            .collect();

        // take block and register into simple shape
        let block = env.node_weight_mut(internal_id).and_then(Block::take)?;

        // comput next subset
        blocks.remove(internal_id);

        let next_subset = CFGSubset::new(next_entries, entries, blocks);

        // create shape
        let kind = SimpleShape {
            internal: block,
            branches_out: shape_branches_out,
        };

        let shape = Shape {
            id: shape_id,
            kind: ShapeKind::Simple(kind),
            next: None,
        };

        Some((shape, next_subset))
    }
    fn make_loop<L, C>(
        self,
        env: &mut GraphEnv<L, C>,
    ) -> Option<(Shape<L, C>, Self)> {
        let CFGSubset {
            blocks,
            entries,
            next_entries,
        } = self;

        // generate loop shape id
        let shape_id = env.next_shape_id();

        // find all blocks than reaches entry blocks
        let mut inner_blocks = env.empty_block_set();

        let mut queue = entries.clone();
        while let Some(block_id) = queue.take_one() {
            if !inner_blocks.contains(block_id) {
                inner_blocks.insert(block_id);
                for incoming in env.blocks_in(block_id, blocks) {
                    queue.insert(incoming);
                }
            }
        }

        assert!(!inner_blocks.is_empty());

        // populate next_entries
        next_entries.extend(
            inner_blocks
                .iter()
                .flat_map(|curr_id| env.blocks_out(curr_id, blocks))
                .filter(|b| !inner_blocks.contains(*b)),
        );

        // add continue branching
        for entry in entries.iter() {
            for from_id in inner_blocks.iter() {
                if let Some(branch) = env.find_branch_mut(from_id, entry) {
                    branch.solipsize(entry, FlowType::Continue, shape_id);
                }
            }
        }
        // add break branching
        for next_entry in next_entries.iter() {
            for from_id in inner_blocks.iter() {
                if let Some(branch) = env.find_branch_mut(from_id, next_entry) {
                    branch.solipsize(next_entry, FlowType::Break, shape_id);
                }
            }
        }

        // remove inner blocks
        for block in inner_blocks.iter() {
            blocks.remove(block);
        }

        // recursivly creates inner shape
        let inner_shape = {
            let mut inner_next_entries = env.empty_block_set();
            let inner_subset = CFGSubset {
                blocks: &mut inner_blocks,
                entries: &mut *entries,
                next_entries: &mut inner_next_entries,
            };
            process(inner_subset, env)?
        };

        let next_subset = CFGSubset::new(next_entries, entries, blocks);

        let shape = Shape {
            id: shape_id,
            kind: ShapeKind::Loop(LoopShape {
                inner: Box::new(inner_shape),
            }),
            next: None,
        };

        Some((shape, next_subset))
    }

    fn find_independent_groups<L, C>(
        &self,
        env: &GraphEnv<L, C>,
    ) -> IndepSetMap {
        use std::ops::AddAssign;

        #[derive(Debug, Clone, Copy, Eq, PartialEq)]
        enum Source {
            Only(BlockId),
            MoreThanOne,
        }
        impl AddAssign for Source {
            fn add_assign(&mut self, other: Source) {
                if let (Source::Only(s1), Source::Only(s2)) = (&self, &other) {
                    if *s1 == *s2 {
                        return;
                    }
                }

                *self = Source::MoreThanOne;
            }
        }

        let mut sources: HashMap<BlockId, Source> = HashMap::new();

        let mut queue: VecDeque<(Source, BlockId)> =
            self.entries.iter().map(|e| (Source::Only(e), e)).collect();

        while let Some((entry, block)) = queue.pop_front() {
            let mut src = match sources.get(&block) {
                Some(src) => *src,
                None => entry,
            };
            src += entry;
            sources.insert(block, src);

            for next_block in env.blocks_out(block, &*self.blocks) {
                match sources.get(&next_block) {
                    Some(src) if *src != entry => {
                        queue.push_back((entry, next_block));
                    }
                    None => {
                        queue.push_back((entry, next_block));
                    }
                    _ => {}
                }
            }
        }

        let mut indep_groups: IndepSetMap = self
            .entries
            .iter()
            .map(|e| (e, env.empty_block_set()))
            .collect();

        for (block, src) in sources.iter() {
            if let Source::Only(entry) = src {
                if let Some(set) = indep_groups.get_mut(entry) {
                    set.insert(*block);
                }
            }
        }

        indep_groups
    }

    fn make_multiple<L, C>(
        self,
        entry_type: EntryType,
        indep_groups: &mut IndepSetMap,
        env: &mut GraphEnv<L, C>,
    ) -> Option<(Shape<L, C>, Self)> {
        let CFGSubset {
            blocks,
            entries,
            next_entries,
        } = self;
        // generate multi shape id
        let shape_id = env.next_shape_id();
        let mut handled = HashMap::new();
        let mut break_count = 0;

        let mut curr_targets = env.empty_block_set();
        // populate next entries
        for (_, targets) in indep_groups.iter_mut() {
            for curr_id in targets.iter() {
                curr_targets.clear();
                curr_targets.extend(env.blocks_out(curr_id, blocks));

                for next_entry in
                    curr_targets.iter().filter(|id| !targets.contains(*id))
                {
                    next_entries.insert(next_entry);
                    if let Some(branch) =
                        env.find_branch_mut(curr_id, next_entry)
                    {
                        branch.solipsize(next_entry, FlowType::Break, shape_id);
                        break_count += 1;
                    }
                }
            }
        }

        for (entry, targets) in indep_groups.iter_mut() {
            if targets.is_empty() {
                next_entries.insert(*entry);
                continue;
            }

            let inner_shape = {
                let mut inner_next_entries = env.empty_block_set();
                let mut single_entry = env.empty_block_set();
                single_entry.insert(*entry);

                let subset = CFGSubset {
                    blocks: targets,
                    entries: &mut single_entry,
                    next_entries: &mut inner_next_entries,
                };
                process(subset, env)?
            };
            handled.insert(
                *entry,
                HandledShape {
                    shape: inner_shape,
                    entry_type,
                },
            );

            for b in targets.iter() {
                blocks.remove(b);
            }
        }

        let shape = Shape {
            id: shape_id,
            kind: ShapeKind::Multi(MultipleShape {
                handled,
                break_count,
            }),
            next: None,
        };
        let next_subset = CFGSubset::new(next_entries, entries, blocks);

        Some((shape, next_subset))
    }
}

impl<L, C> GraphEnv<L, C> {
    fn next_shape_id(&mut self) -> ShapeId {
        self.shape_id_gen.next_shape_id()
    }

    fn blocks_out<'a>(
        &'a self,
        block_id: BlockId,
        blocks: &'a BlockSet,
    ) -> impl Iterator<Item = BlockId> + 'a {
        assert!(blocks.contains(block_id));

        self.cfgraph
            .neighbors_directed(block_id, Direction::Outgoing)
            .filter(move |id| {
                if !blocks.contains(*id) {
                    return false;
                }
                self.find_raw_branch(block_id, *id).is_some()
            })
    }

    fn blocks_in<'a>(
        &'a self,
        block_id: BlockId,
        blocks: &'a BlockSet,
    ) -> impl Iterator<Item = BlockId> + 'a {
        assert!(blocks.contains(block_id));

        self.cfgraph
            .neighbors_directed(block_id, Direction::Incoming)
            .filter(move |id| {
                if !blocks.contains(*id) {
                    return false;
                }
                self.find_raw_branch(*id, block_id).is_some()
            })
    }

    fn find_raw_branch(&self, a: BlockId, b: BlockId) -> Option<&Branch<C>> {
        self.find_branch(a, b).filter(|e| {
            if let Branch::Raw(_) = e {
                true
            } else {
                false
            }
        })
    }

    fn find_branch(&self, a: BlockId, b: BlockId) -> Option<&Branch<C>> {
        let edge = self.cfgraph.find_edge(a, b)?;
        self.cfgraph.edge_weight(edge)
    }

    fn find_raw_branch_mut(
        &mut self,
        a: BlockId,
        b: BlockId,
    ) -> Option<&mut Branch<C>> {
        self.find_branch_mut(a, b).filter(|e| {
            if let Branch::Raw(_) = e {
                true
            } else {
                false
            }
        })
    }

    fn find_branch_mut(
        &mut self,
        a: BlockId,
        b: BlockId,
    ) -> Option<&mut Branch<C>> {
        let edge = self.cfgraph.find_edge(a, b)?;
        self.cfgraph.edge_weight_mut(edge)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_it() {
        let mut graph: CFGraph<&'static str, &'static str> = CFGraph::default();
        let shape_id_gen = ShapeIdGen::default();

        let a = graph.add_node(Block::Raw("a"));
        let b = graph.add_node(Block::Raw("b"));
        let c = graph.add_node(Block::Raw("c"));
        // let d = graph.add_node(Block::Raw("d"));

        graph.add_edge(a, b, Branch::Raw(Some("true")));
        graph.add_edge(a, c, Branch::Raw(Some("false")));
        graph.add_edge(b, c, Branch::Raw(Some("false")));
        // graph.add_edge(c, b, Branch::Raw(Some("false")));
        // graph.add_edge(c, d, Branch::Raw(Some("break")));

        let mut entries = empty_block_set(&graph);
        let mut next_entries = empty_block_set(&graph);
        let mut blocks = empty_block_set(&graph);
        for n in graph.node_indices() {
            blocks.insert(n);
        }

        entries.insert(a);

        let subset = CFGSubset {
            blocks: &mut blocks,
            entries: &mut entries,
            next_entries: &mut next_entries,
        };
        let mut env = GraphEnv {
            shape_id_gen,
            cfgraph: graph,
        };

        let shape = process(subset, &mut env);

        eprintln!("{:#?}", shape.unwrap());

        assert!(true);
    }
}
