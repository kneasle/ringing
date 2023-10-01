use std::ops::{Deref, Range};

use bellframe::RowBuf;
use itertools::Itertools;

use crate::{
    graph::ChunkId,
    parameters::{
        Call, CallVec, MethodId, MethodIdx, MethodVec, MusicType, MusicTypeId, MusicTypeVec,
        Parameters,
    },
    utils::PerPartLength,
};

/// To allow the GUI to edit the [`Parameters`] directly, it is highly useful to be able to
/// include methods/calls/music types in a set of [`Parameters`] but then mark them as 'unused'.
/// However, the graph build and search algorithms only care about which methods are used, so
/// in a [`Query`] we can do that filtering and provide cheap access to only used
/// methods/calls/music types.
// TODO: Fully remove this
#[derive(Debug)]
pub(crate) struct Query {
    pub parameters: Parameters,
    pub methods: MethodVec<Method>,
    pub calls: CallVec<Call>,
    pub music_types: MusicTypeVec<MusicType>,
}

// TODO: Remove this and calculate the values manually
#[derive(Debug)]
pub(crate) struct Method {
    pub inner: crate::parameters::Method,
}

impl Query {
    pub(crate) fn get_method_by_id(&self, id: MethodId) -> MethodIdx {
        self.methods
            .iter()
            .find_position(|m| m.id == id)
            .unwrap()
            .0
            .into()
    }

    pub(crate) fn get_music_type_by_id(&self, id: MusicTypeId) -> &MusicType {
        // TODO: if this is a bottleneck, we can optimise this with a hashmap
        self.music_types.iter().find(|mt| mt.id == id).unwrap()
    }

    /// For a given chunk, split that chunk's range into segments where each one falls within a
    /// unique lead.  For example, a chunk with ID `ChunkId { <Little Bob>, 12345678, sub_lead_idx: 2 }`
    /// and length 18 would return the following regions:
    /// ```text
    ///                           12345678
    ///                            1
    ///                  +     +    1
    ///                  |     |     1
    ///                  |     |     1
    ///                  |     |    1
    ///                  |     |   1
    ///                  |     +  1
    ///                  |     +  16482735
    ///                  |     |   1
    ///                  |     |    1
    ///   Original range |     |     1
    ///                  |     |     1
    ///                  |     |    1
    ///                  |     |   1
    ///                  |     +  1
    ///                  |     +  17856342
    ///                  |     |   1
    ///                  |     |    1
    ///                  +     +     1
    ///                      /       1
    ///                     /       1
    ///    Output ranges --/       1
    ///                           1
    /// ```
    pub(crate) fn chunk_lead_regions(
        &self,
        id: &ChunkId,
        length: PerPartLength,
    ) -> Vec<(RowBuf, Range<usize>)> {
        let method = &self.methods[id.method];

        let mut lead_head: RowBuf = id.lead_head.deref().to_owned();
        let mut length_left = length.as_usize();
        let mut sub_lead_idx = id.sub_lead_idx;

        let mut lead_regions = Vec::new();
        while length_left > 0 {
            // Add a region for as much of this lead as we can
            let length_left_in_lead = method.lead_len() - sub_lead_idx;
            let chunk_len = usize::min(length_left_in_lead, length_left);
            let chunk_end = sub_lead_idx + chunk_len;
            lead_regions.push((lead_head.clone(), sub_lead_idx..chunk_end));
            // Move to after this region, moving forward a lead if necessary
            assert!(chunk_len <= method.lead_len());
            length_left -= chunk_len;
            sub_lead_idx += chunk_len;
            assert!(sub_lead_idx <= method.lead_len());
            if sub_lead_idx == method.lead_len() {
                // Next chunk starts in a new lead, so update the lead head accordingly
                sub_lead_idx = 0;
                lead_head *= method.lead_head();
            }
        }
        lead_regions
    }
}

impl Deref for Query {
    type Target = Parameters;

    fn deref(&self) -> &Self::Target {
        &self.parameters
    }
}

impl Deref for Method {
    type Target = crate::parameters::Method;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/////////////////////////
// BUILDING EXTRA DATA //
/////////////////////////

impl Query {
    pub(crate) fn new(parameters: Parameters) -> Self {
        // Filter methods and calls
        let used_methods = parameters
            .maybe_unused_methods
            .iter()
            .filter(|m| m.used)
            .cloned()
            .collect_vec();
        let used_calls: CallVec<_> = parameters
            .maybe_unused_calls
            .iter()
            .filter(|c| c.used)
            .cloned()
            .collect();
        let used_music_types: MusicTypeVec<_> = parameters
            .maybe_unused_music_types
            .iter()
            .filter(|mt| mt.used)
            .cloned()
            .collect();

        Self {
            methods: used_methods.into_iter().map(Method::new).collect(),
            calls: used_calls,
            music_types: used_music_types,

            parameters,
        }
    }
}

impl Method {
    fn new(method: crate::parameters::Method) -> Self {
        Self { inner: method }
    }
}
