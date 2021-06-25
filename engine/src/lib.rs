use std::{ops::Range, sync::Arc, thread};

use bellframe::{Bell, Method};
use itertools::Itertools;

// use compose::EngineWorker;
use layout::{Layout, SegmentId};
use segment_table::{SegmentTable, SegmentTableEntry};
use single_method::{single_method_layout, CallSpec, SingleMethodError};

// mod compose;
mod graph;
mod layout;
pub mod mask;
mod music;
mod segment_table;
pub mod single_method;

// Imports used solely by doctests
#[allow(unused_imports)]
use bellframe::Row;

// Top level re-exports for convenience
pub use music::MusicType;

/// The static data required for composition generation.  This data will not be modified and will
/// be shared between threads (once multi-threading is implemented).
#[derive(Debug, Clone)]
pub struct Engine {
    config: Config,
    len_range: Range<usize>,
    segment_table: SegmentTable,
}

impl Engine {
    /// Creates an `Engine` from a course [`Layout`].
    pub fn from_layout(
        config: Config,
        len_range: Range<usize>,
        layout: Layout,
        music: Vec<MusicType>,
    ) -> Self {
        println!("{:#?}", layout);

        Self {
            config,
            len_range,
            segment_table: SegmentTable::from_segments(
                &layout.segments,
                &layout.fixed_bells,
                &music,
            ),
        }
    }

    /// Creates an `Engine` to generate compositions of a single method.
    pub fn single_method(
        // General params
        config: Config,
        len_range: Range<usize>,
        // Method or course structure
        method: &Method,
        calls: &[CallSpec],
        non_fixed_bells: &[Bell],
        plain_lead_positions: Option<Vec<String>>,
        // Music
        music: Vec<MusicType>,
    ) -> Result<Self, SingleMethodError> {
        let layout = single_method_layout(method, plain_lead_positions, calls, non_fixed_bells)?;
        Ok(Self::from_layout(config, len_range, layout, music))
    }

    /// Generate the compositions
    pub fn compose(self) {
        let arc_self = Arc::from(self);

        let threads = (0usize..1)
            .map(|i| {
                let thread_arc = arc_self.clone();
                thread::Builder::new()
                    .name(format!("Worker{}", i))
                    .spawn(move || {
                        /*
                        let mut worker = EngineWorker::from_engine(thread_arc, i);
                        worker.compose();
                        */
                        panic!()
                    })
                    .unwrap()
            })
            .collect_vec();

        // Stop the main thread until all workers have returned
        for t in threads {
            t.join().unwrap();
        }
    }

    /// Gets the table corresponding to a given ID
    pub(crate) fn get_seg_table(&self, seg_id: SegmentId) -> &SegmentTableEntry {
        &self.segment_table.entries[usize::from(seg_id)]
    }

    pub fn debug(&self) {
        for (i, t) in self.segment_table.entries.iter().enumerate() {
            println!("Seg {}:", i);
            println!("  len: {}", t.length);
            println!("  falseness:");
            for (r, s) in &t.false_segments {
                println!("    {}: {}", s.idx, r);
            }
        }
    }
}

/// General configuration parameters for an [`Engine`].  These do not form parts of the composition
/// specification, instead changing how the [`Engine`] operates.
#[derive(Debug, Clone)]
pub struct Config {
    /// The number of compositions that the [`Engine`] will generate before terminating.  These
    /// `num_comps` compositions are guaranteed to be optimal.
    pub num_comps: usize,
    /// How many threads will be used to generate the best composition.  If set to `None`, this
    /// will use the number of available CPU cores.
    pub num_threads: Option<usize>,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            num_comps: 1,
            num_threads: None,
        }
    }
}
