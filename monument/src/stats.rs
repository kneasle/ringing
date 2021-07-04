use std::ops::AddAssign;

/// Statistics about Monument's progress
#[derive(Debug, Clone, Default)]
pub struct Stats {
    /// The number of nodes which were _considered_ - i.e. had `explore_node` called on them.
    pub nodes_considered: usize,
    /// The number of nodes which were proven false
    pub nodes_proven_false: usize,
    /// The number of nodes which passed the falseness test and were loaded into the graph.  The
    /// number of node unloads must be the same as this.
    pub nodes_loaded: usize,
    /// The total number of compositions found (most will be ignore)
    pub comps_found: usize,
}

impl Stats {
    /// Creates a `Stats` representing no progress
    pub(crate) fn zero() -> Self {
        Self::default()
    }

    #[inline(always)]
    pub(crate) fn on_find_comp(&mut self) {
        self.comps_found += 1;
    }

    #[inline(always)]
    pub(crate) fn on_node_consider(&mut self) {
        self.nodes_considered += 1;
    }
}

impl AddAssign for Stats {
    fn add_assign(&mut self, rhs: Self) {
        self.nodes_considered += rhs.nodes_considered;
        self.nodes_proven_false += rhs.nodes_proven_false;
        self.nodes_loaded += rhs.nodes_loaded;
        self.comps_found += rhs.comps_found;
    }
}
