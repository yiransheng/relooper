use std::collections::{HashMap, VecDeque};

use petgraph::Direction;
use smallvec::SmallVec;

use crate::shapes::*;
use crate::types::*;

type SVec<T> = SmallVec<[T; 4]>;

type IndepSetMap = HashMap<BlockId, BlockSet>;

struct CFGSubset<'a, 'g, L, C> {
    blocks: &'a mut BlockSet,
    entries: &'a mut BlockSet,
    next_entries: &'a mut BlockSet,

    shape_id_gen: &'g mut ShapeIdGen,
    cfgraph: &'g mut CFGraph<L, C>,
}

fn empty_block_set<L, C>(g: &CFGraph<L, C>) -> BlockSet {
    BlockSet::new_empty(g.node_count())
}

fn process<'a, 'g, L, C>(
    mut subset: CFGSubset<'a, 'g, L, C>,
) -> Option<(Shape<L, C>, &'g mut ShapeIdGen, &'g mut CFGraph<L, C>)> {
    let mut shape: Option<Shape<L, C>> = None;
    let mut has_ret = false;
    let mut prev: &mut Link<L, C> = &mut None;
    let mut multi_entry_type: EntryType = EntryType::Checked;

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
                return shape.map(move |s| {
                    (s, next_subset.shape_id_gen, next_subset.cfgraph)
                });
            } else {
                subset = next_subset;
                continue;
            }
        }};
    }

    loop {
        if subset.entries.len() == 0 {
            return None;
        }
        if subset.entries.len() == 1 {
            let single_entry = subset.entries.sample_one().unwrap();
            // Case 1: no branch returns to entry
            if subset.blocks_in(single_entry).count() == 0 {
                make!(subset.make_simple());
            }

            // Case 2: some branch(es) returns to entry
            make!(subset.make_loop());
        }

        let mut indep_groups = subset.find_independent_groups();

        let has_multi_entry =
            indep_groups.values().filter(|g| !g.is_empty()).count() > 0;

        if has_multi_entry {
            make!(subset.make_multiple(&mut indep_groups, multi_entry_type));
        } else {
            make!(subset.make_loop());
        }
    }
}

impl<'a, 'g, L, C> CFGSubset<'a, 'g, L, C> {
    fn make_simple(mut self) -> Option<(Shape<L, C>, CFGSubset<'a, 'g, L, C>)> {
        // generate new shape id
        let shape_id: ShapeId = self.shape_id_gen.next_shape_id();

        // single entry (entries.len() == 1 checked in caller)
        let internal_id: BlockId = self.entries.sample_one()?;

        // populate next entries
        let mut next_entries =
            ::std::mem::replace(self.next_entries, BlockSet::new_empty(0));
        for target_id in self.blocks_out(internal_id) {
            next_entries.insert(target_id);
        }
        ::std::mem::replace(self.next_entries, next_entries);

        let mut targets: SVec<BlockId> = self
            .cfgraph
            .neighbors_directed(internal_id, Direction::Outgoing)
            .collect();

        // solipsize/process branches
        for t in targets.iter().cloned() {
            if let Some(branch) = self.find_branch_across_shapes(internal_id, t)
            {
                branch.solipsize(t, FlowType::Direct, shape_id);
            }
        }

        // take processed branches (leaving dummy Branch::Registered in graph)
        let shape_branches_out: HashMap<_, _> = targets
            .drain()
            .filter_map(|t| {
                if let Some(branch) =
                    self.find_branch_across_shapes(internal_id, t)
                {
                    branch.take().map(|b| (t, b))
                } else {
                    None
                }
            })
            .collect();

        // take block and register into simple shape
        let block = self
            .cfgraph
            .node_weight_mut(internal_id)
            .and_then(Block::take)?;

        // comput next subset
        self.blocks.remove(internal_id);

        let mut next_subset = CFGSubset {
            blocks: self.blocks,
            entries: self.entries,
            next_entries: self.next_entries,
            cfgraph: self.cfgraph,
            shape_id_gen: self.shape_id_gen,
        };
        next_subset.swap_entries();

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

    fn make_loop(mut self) -> Option<(Shape<L, C>, Self)> {
        // generate loop shape id
        let shape_id = self.shape_id_gen.next_shape_id();

        // find all blocks than reaches entry blocks
        let mut inner_blocks = self.empty_block_set();

        // locally allocated temp BlockSet, will be reused
        // a few times
        let mut tmp_set = self.entries.clone();
        while let Some(block_id) = tmp_set.take_one() {
            if !inner_blocks.contains(block_id) {
                inner_blocks.insert(block_id);
                for incoming in self.blocks_in(block_id) {
                    tmp_set.insert(incoming);
                }
            }
        }

        assert!(inner_blocks.len() > 0);

        macro_rules! temp_move_blocks {
            ($field: expr, $b: block) => {{
                tmp_set = ::std::mem::replace($field, tmp_set);
                $b;
                tmp_set = ::std::mem::replace($field, tmp_set);
            }};
        }

        // populate next_entries
        temp_move_blocks!(self.next_entries, {
            for curr_id in inner_blocks.iter() {
                for possible in self.blocks_out(curr_id) {
                    if !inner_blocks.contains(possible) {
                        tmp_set.insert(possible);
                    }
                }
            }
        });
        // add continue branching
        temp_move_blocks!(self.entries, {
            for entry in tmp_set.iter() {
                for from_id in inner_blocks.iter() {
                    if let Some(branch) = self.find_branch(from_id, entry) {
                        branch.solipsize(entry, FlowType::Continue, shape_id);
                    }
                }
            }
        });
        // add break branching
        temp_move_blocks!(self.next_entries, {
            for next_entry in tmp_set.iter() {
                for from_id in inner_blocks.iter() {
                    if let Some(branch) = self.find_branch(from_id, next_entry)
                    {
                        branch.solipsize(
                            next_entry,
                            FlowType::Continue,
                            shape_id,
                        );
                    }
                }
            }
        });

        // remove inner blocks
        for block in inner_blocks.iter() {
            self.blocks.remove(block);
        }

        // recursivly creates inner shape
        let (inner_shape, shape_id_gen, cfgraph) = {
            let mut inner_next_entries = self.empty_block_set();
            let inner_subset = CFGSubset {
                blocks: &mut inner_blocks,
                entries: self.entries,
                next_entries: &mut inner_next_entries,

                shape_id_gen: self.shape_id_gen,
                cfgraph: self.cfgraph,
            };
            process(inner_subset)?
        };

        let mut next_subset = CFGSubset {
            shape_id_gen,
            blocks: self.blocks,
            entries: self.entries,
            next_entries: self.next_entries,
            cfgraph,
        };
        next_subset.swap_entries();

        let shape = Shape {
            id: shape_id,
            kind: ShapeKind::Loop(LoopShape {
                inner: Box::new(inner_shape),
            }),
            next: None,
        };

        Some((shape, next_subset))
    }

    fn find_independent_groups(&self) -> IndepSetMap {
        use std::ops::AddAssign;

        #[derive(Debug, Clone, Copy)]
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

        let mut sources: HashMap<BlockId, Source> =
            self.entries.iter().map(|e| (e, Source::Only(e))).collect();

        let mut queue: VecDeque<(BlockId, BlockId)> =
            self.entries.iter().map(|e| (e, e)).collect();

        while let Some((entry, block)) = queue.pop_front() {
            let mut src = match sources.get(&block) {
                Some(src) => *src,
                None => Source::Only(entry),
            };
            src += Source::Only(entry);
            sources.insert(block, src);

            if let Source::MoreThanOne = src {
                continue;
            }

            for next_block in self.blocks_out(block) {
                match sources.get(&next_block) {
                    Some(Source::MoreThanOne) => {}
                    Some(Source::Only(s)) if *s == entry => {}
                    Some(Source::Only(_)) => {
                        sources.insert(next_block, Source::MoreThanOne);
                    }
                    None => {
                        queue.push_back((entry, next_block));
                    }
                }
            }
        }

        let mut indep_groups: IndepSetMap = self
            .entries
            .iter()
            .map(|e| (e, self.empty_block_set()))
            .collect();

        for (block, src) in sources.iter() {
            if let Source::Only(entry) = src {
                indep_groups.get_mut(entry).map(|set| {
                    set.insert(*block);
                });
            }
        }

        indep_groups
    }

    fn make_multiple(
        mut self,
        indep_groups: &mut IndepSetMap,
        entry_type: EntryType,
    ) -> Option<(Shape<L, C>, Self)> {
        // generate multi shape id
        let shape_id = self.shape_id_gen.next_shape_id();
        let mut handled = HashMap::new();
        let mut break_count = 0;

        let mut next_entries =
            ::std::mem::replace(self.next_entries, BlockSet::new_empty(0));

        let mut curr_targets = self.empty_block_set();
        for (entry, targets) in indep_groups.iter_mut() {
            if targets.is_empty() {
                next_entries.insert(*entry);
                continue;
            }
            self.blocks.remove(*entry);

            for curr_id in targets.iter() {
                self.blocks.remove(curr_id);
                curr_targets.clear();

                for next_entry in self.blocks_out(curr_id) {
                    curr_targets.insert(next_entry);
                }

                for next_entry in
                    curr_targets.iter().filter(|id| !targets.contains(*id))
                {
                    next_entries.insert(next_entry);
                    if let Some(branch) = self.find_branch(curr_id, next_entry)
                    {
                        branch.solipsize(next_entry, FlowType::Break, shape_id);
                        break_count += 1;
                    }
                }
            }
        }
        ::std::mem::replace(self.next_entries, next_entries);

        let n_nodes = self.cfgraph.node_count();
        let CFGSubset {
            entries,
            next_entries,
            mut shape_id_gen,
            mut cfgraph,
            blocks,
        } = self;

        for (entry, targets) in indep_groups.iter_mut() {
            if targets.is_empty() {
                continue;
            }

            let (inner_shape, shape_id_gen_, cfgraph_) = {
                let mut inner_next_entries = BlockSet::new_empty(n_nodes);
                let mut single_entry = BlockSet::new_empty(n_nodes);
                single_entry.insert(*entry);

                let subset = CFGSubset {
                    blocks: targets,
                    entries: &mut single_entry,
                    next_entries: &mut inner_next_entries,
                    cfgraph,
                    shape_id_gen,
                };
                process(subset)?
            };
            cfgraph = cfgraph_;
            shape_id_gen = shape_id_gen_;

            handled.insert(
                *entry,
                HandledShape {
                    shape: inner_shape,
                    entry_type,
                },
            );
        }

        let shape = Shape {
            id: shape_id,
            kind: ShapeKind::Multi(MultipleShape {
                handled,
                break_count,
            }),
            next: None,
        };
        let mut next_subset = CFGSubset {
            entries,
            next_entries,
            blocks,
            cfgraph,
            shape_id_gen,
        };
        next_subset.swap_entries();

        Some((shape, next_subset))
    }

    fn swap_entries(&mut self) {
        ::std::mem::swap(self.entries, self.next_entries);
        self.next_entries.clear();
    }

    fn blocks_out<'b>(
        &'b self,
        block: BlockId,
    ) -> impl Iterator<Item = BlockId> + 'b {
        assert!(self.blocks.contains(block));

        self.cfgraph
            .neighbors_directed(block, Direction::Outgoing)
            .filter(move |id| {
                let id = *id;
                if !self.blocks.contains(id) {
                    return false;
                }
                let edge = match self.cfgraph.find_edge(block, id) {
                    Some(edge) => edge,
                    _ => return false,
                };
                if let Some(Branch::Raw(_)) = self.cfgraph.edge_weight(edge) {
                    true
                } else {
                    false
                }
            })
    }

    fn blocks_in<'b>(
        &'b self,
        block: BlockId,
    ) -> impl Iterator<Item = BlockId> + 'b {
        assert!(self.blocks.contains(block));

        self.cfgraph
            .neighbors_directed(block, Direction::Incoming)
            .filter(move |id| {
                let id = *id;
                if !self.blocks.contains(id) {
                    return false;
                }
                let edge = match self.cfgraph.find_edge(id, block) {
                    Some(edge) => edge,
                    _ => return false,
                };
                if let Some(Branch::Raw(_)) = self.cfgraph.edge_weight(edge) {
                    true
                } else {
                    false
                }
            })
    }

    fn find_branch(
        &mut self,
        a: BlockId,
        b: BlockId,
    ) -> Option<&mut Branch<C>> {
        if !self.blocks.contains(a) {
            return None;
        }

        let edge = self.cfgraph.find_edge(a, b)?;
        self.cfgraph.edge_weight_mut(edge)
    }

    fn find_branch_across_shapes(
        &mut self,
        a: BlockId,
        b: BlockId,
    ) -> Option<&mut Branch<C>> {
        let edge = self.cfgraph.find_edge(a, b)?;
        self.cfgraph.edge_weight_mut(edge)
    }

    fn empty_block_set(&self) -> BlockSet {
        BlockSet::new_empty(self.cfgraph.node_count())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_it() {
        let mut graph: CFGraph<&'static str, &'static str> = CFGraph::default();
        let mut shape_id_gen = ShapeIdGen::default();

        let a = graph.add_node(Block::Raw("a"));
        let b = graph.add_node(Block::Raw("b"));
        let c = graph.add_node(Block::Raw("c"));

        graph.add_edge(a, b, Branch::Raw(Some("true")));
        graph.add_edge(b, c, Branch::Raw(Some("false")));
        graph.add_edge(c, b, Branch::Raw(Some("false")));

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

            cfgraph: &mut graph,
            shape_id_gen: &mut shape_id_gen,
        };

        let shape = process(subset).map(|x| x.0);

        println!("{:#?}", shape);

        assert!(false);
    }
}
