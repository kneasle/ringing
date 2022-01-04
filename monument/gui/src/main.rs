mod params;
mod utils;

use std::{
    collections::HashSet,
    hash::Hash,
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::{sync_channel, Receiver, SyncSender},
        Arc, Mutex,
    },
    time::{Duration, Instant},
};

use bellframe::MethodLib;
use eframe::{
    egui::{self, CtxRef},
    epi,
};
use itertools::Itertools;
use monument::{Comp, Config, Progress, Query, QueryUpdate};
use number_prefix::NumberPrefix;
use params::Params;

const COMP_REQUEST_LIMIT: usize = 100;

type SearchRequest = (Arc<dyn epi::RepaintSignal>, Arc<Search>);

fn main() {
    // Initialise logging
    pretty_logger::init(
        pretty_logger::Destination::Stderr,
        log::LogLevelFilter::Info,
        pretty_logger::Theme::default(),
    )
    .unwrap();

    // Create channels/atomics used for communicating with the worker thread
    let (query_tx, query_rx) = sync_channel(COMP_REQUEST_LIMIT);
    let abort_flag = Arc::new(AtomicBool::new(false));
    // Spawn (then detach) the composing thread
    let abort_flag_for_search_thread = abort_flag.clone();
    std::thread::spawn(move || compose_thread(query_rx, abort_flag_for_search_thread));
    // Start the app
    eframe::run_native(
        Box::new(MonumentApp::new(query_tx, abort_flag)),
        eframe::NativeOptions::default(),
    );
}

/// Singleton struct containing all data required to run a single session of Monument's GUI
struct MonumentApp {
    params: Params,
    cc_method_lib: MethodLib,
    searches: Vec<Arc<Search>>,
    request_tx: SyncSender<SearchRequest>,
    /// If set to `true`, the current search will abort
    abort_flag: Arc<AtomicBool>,
}

/// A single (possibly in progress) search for compositions.
struct Search {
    query: Arc<Query>,
    mutex: Mutex<SearchMutex>,
}

/// Struct to hold the mutex values.  We have to share the mutex because writing directly to the
/// value of a mutex (like `*guard = value`) causes a deadlock.  So this way, we only set each
/// field independently and all is fine.
struct SearchMutex {
    comps: Vec<Arc<Comp>>,
    status: Status,
}

/// The `Status` of a [`Search`]
enum Status {
    InProgress {
        start_time: Instant,
        progress: Progress,
    },
    Completed(Duration),
    Failed(monument::graph::BuildError),
}

impl epi::App for MonumentApp {
    fn name(&self) -> &str {
        "Monument"
    }

    fn update(&mut self, ctx: &CtxRef, frame: &mut epi::Frame<'_>) {
        let window_rect = ctx.available_rect();
        // Fill background
        ctx.layer_painter(egui::LayerId::background()).rect_filled(
            window_rect,
            0.0,
            ctx.style().visuals.window_fill(),
        );

        fn add_panel(
            ctx: &CtxRef,
            left_factor: f32,
            right_factor: f32,
            id: impl Hash,
            mut add: impl FnMut(&mut egui::Ui),
        ) {
            assert!(0.0 <= left_factor);
            assert!(left_factor <= right_factor);
            assert!(right_factor <= 1.0);

            let window_rect = ctx.available_rect();
            let mut panel_rect = window_rect;
            panel_rect.set_left(egui::lerp(window_rect.x_range(), left_factor));
            panel_rect.set_right(egui::lerp(window_rect.x_range(), right_factor));
            panel_rect = panel_rect.shrink(4.0);
            let mut ui = egui::Ui::new(
                ctx.clone(),
                egui::LayerId::background(),
                egui::Id::new(id),
                panel_rect,
                panel_rect,
            );
            add(&mut ui);
        }

        add_panel(ctx, 0.0, 0.25, "params panel", |ui| {
            if self.params.draw_gui(ui, &self.cc_method_lib) {
                self.on_search_button_click(frame.repaint_signal());
            }
        });
        add_panel(ctx, 0.25, 0.50, "queries panel", |ui| {
            self.draw_searches_panel(ui);
        });
        add_panel(ctx, 0.50, 0.75, "comps panel", |ui| {
            self.draw_comps_panel(ui);
        });
        add_panel(ctx, 0.75, 1.00, "comp view panel", |ui| {
            utils::centered_heading(ui, "Composition View");
            egui::ScrollArea::vertical()
                .auto_shrink([false, false])
                .show(ui, |ui| {
                    for _ in 0..100 {
                        ui.label("The quick brown fox jumped over the lazy dog.");
                    }
                });
        });
    }
}

impl MonumentApp {
    fn new(request_tx: SyncSender<SearchRequest>, abort_flag: Arc<AtomicBool>) -> Self {
        Self {
            params: Params::default(),
            cc_method_lib: MethodLib::cc_lib().expect("couldn't load CC methods"),
            searches: vec![],
            request_tx,
            abort_flag,
        }
    }

    fn draw_searches_panel(&mut self, ui: &mut egui::Ui) {
        utils::centered_heading(ui, "Searches");
        if self.searches.is_empty() {
            ui.label("None yet; click 'Search!' to start a search");
            return;
        }
        for search in &self.searches {
            let mutex = search.mutex.lock().unwrap();
            match &mutex.status {
                Status::InProgress {
                    start_time,
                    progress,
                } => {
                    ui.separator();
                    ui.add(egui::Label::new("In progress:").strong());
                    egui::Grid::new("progress_grid").show(ui, |ui| {
                        ui.label("Time elapsed:");
                        ui.label(format!("{:.2?}", Instant::now() - *start_time));
                        ui.end_row();

                        ui.label("Comps found:");
                        ui.label(format!("{}/{}", mutex.comps.len(), search.query.num_comps));
                        ui.end_row();

                        ui.label("Prefixes searched:");
                        ui.label(format_big_num(progress.iter_count));
                        ui.end_row();

                        ui.label("Prefixes in queue:");
                        ui.label(format_big_num(progress.queue_len));
                        ui.end_row();

                        ui.label("Average prefix length:");
                        ui.label(format!("{:.0}", progress.avg_length));
                        ui.end_row();

                        ui.label("Max prefix length:");
                        ui.label(progress.max_length);
                        ui.end_row();
                    });
                    if self.abort_flag.load(Ordering::Relaxed) {
                        // If the abort flag has already been set but the search hasn't
                        // terminated yet, then replace the 'abort' button with some text so
                        // the user has some feedback
                        ui.label("Aborting...");
                    } else if ui.button("Abort").clicked() {
                        self.abort_flag.store(true, Ordering::Relaxed);
                    }
                }
                Status::Completed(duration) => {
                    ui.label(format!(
                        "{} comps found in {:.2?}",
                        mutex.comps.len(),
                        duration
                    ));
                }
                Status::Failed(error) => {
                    // TODO: Implement `Display` for `BuildError`
                    ui.label(format!("Graph building failed: {:?}", error));
                }
            }
        }
    }

    fn draw_comps_panel(&mut self, ui: &mut egui::Ui) {
        // Get a list of deduplicated compositions.
        //
        // TODO: Would it be better just to hold locks on all the `Search`'s comp lists, rather
        // than store all comps inside `Arc`s?
        let mut comps = HashSet::<Arc<Comp>>::new();
        for search in &self.searches {
            for c in &search.mutex.lock().unwrap().comps {
                comps.insert(c.clone());
            }
        }
        // Sort the compositions in descending order of goodness (so the best are at the top)
        let mut comps = comps.into_iter().collect_vec();
        comps.sort_by(|c1, c2| {
            // PERF: Ideally we'd tag all comps with a unique ID and use that for tiebreaks
            // since that will be much more efficient.
            c1.avg_score
                .cmp(&c2.avg_score)
                .then_with(|| c1.links.cmp(&c2.links))
                .reverse()
        });

        // Draw GUI
        utils::centered_heading(ui, "Compositions");
        if comps.is_empty() {
            ui.label("No compositions yet");
        } else {
            ui.label(format!("{} comps found", comps.len()));
            // Draw a grid for the comps
            egui::ScrollArea::vertical()
                .auto_shrink([false, false]) // Fill width and height
                .show(ui, |ui| {
                    egui::Grid::new("comp grid")
                        .striped(true)
                        .spacing([20.0, 5.0])
                        .show(ui, |ui| {
                            // Header row
                            let columns = ["Length", "Score", "String"];
                            for column_name in columns {
                                ui.add(egui::Label::new(column_name).strong());
                            }
                            ui.end_row();
                            // Comp rows
                            for c in &comps {
                                ui.label(c.length);
                                ui.label(format!("{:.2}", c.total_score));
                                ui.label(c);
                                ui.end_row();
                            }
                        });
                });
        }
    }

    /// Function called when the `Search!` button is clicked.  This sends a request to the worker
    /// thread to generate a new composition.
    fn on_search_button_click(&mut self, repaint_signal: Arc<dyn epi::RepaintSignal>) {
        match self.params.to_search(&self.cc_method_lib) {
            Ok(search) => {
                let search = Arc::new(search);
                self.searches.push(search.clone());
                self.request_tx
                    .send((repaint_signal, search))
                    .expect("Comp request channel closed");
            }
            Err(e) => println!("Query error: {:?}", e),
        }
    }
}

/// Thread which runs in parallel to the main thread, running incoming query requests and handling
/// the results without blocking the UI.
fn compose_thread(request_channel: Receiver<SearchRequest>, abort_flag: Arc<AtomicBool>) {
    while let Ok((repaint, search)) = request_channel.recv() {
        // Run the search, requesting a UI repaint whenever comps are added
        println!("Running search");
        let config = Config::default();
        let graph = match search.query.unoptimised_graph(&config) {
            Ok(graph) => graph,
            Err(e) => {
                search.mutex.lock().unwrap().status = Status::Failed(e);
                repaint.request_repaint();
                continue;
            }
        };
        let optimised_graphs = search.query.optimise_graph(graph, &config);
        {
            let search = search.clone();
            let repaint = repaint.clone();
            Query::search(
                search.query.clone(),
                optimised_graphs,
                &config,
                move |update: QueryUpdate| {
                    let mut mutex = search.mutex.lock().unwrap();
                    match update {
                        // Add the comp to this query's set of generated comps
                        QueryUpdate::Comp(comp) => mutex.comps.push(Arc::new(comp)),
                        // Update the search progress
                        QueryUpdate::Progress(new_progress) => match &mut mutex.status {
                            Status::InProgress { progress, .. } => *progress = new_progress,
                            Status::Completed(..) | Status::Failed(..) => {
                                panic!("Progress update for completed search")
                            }
                        },
                        // Ignore truncation/abort messages for now
                        QueryUpdate::TruncatingQueue | QueryUpdate::Aborting => (),
                    }
                    // Whatever the update, something changed so we should repaint the GUI
                    repaint.request_repaint();
                },
                abort_flag.clone(),
            );
        }
        // Mark that the search has been completed
        let mut mutex = search.mutex.lock().unwrap();
        let start_time = match &mutex.status {
            Status::InProgress { start_time, .. } => *start_time,
            Status::Completed(..) | Status::Failed(..) => {
                panic!("Can't finish an already completed search")
            }
        };
        mutex.status = Status::Completed(Instant::now() - start_time);
        repaint.request_repaint();
        println!("Search complete");
    }
    println!("Worker thread terminating");
}

fn format_big_num(n: usize) -> String {
    match NumberPrefix::decimal(n as f32) {
        NumberPrefix::Standalone(n) => format!("{:.0}", n),
        NumberPrefix::Prefixed(prefix, factor) => format!("{:.2}{}", factor, prefix),
    }
}
