//! Code for the worker thread which runs Monument's searches

use std::sync::{
    atomic::{AtomicBool, Ordering},
    mpsc::{Receiver, Sender},
    Arc, Mutex,
};

use crate::project::Project;

use eframe::egui;

/// Struct to handle communication with the worker thread which runs queries.
pub struct SearchThreadHandle {
    search_channel_tx: Sender<(Arc<Mutex<Project>>, crate::Parameters, egui::Context)>,
    abort_flag: Arc<AtomicBool>,
}

impl SearchThreadHandle {
    /// Spawns a background worker thread and returns a new `SearchThreadHandle` to communicate
    /// with it
    pub fn new() -> Self {
        let abort_flag = Arc::new(AtomicBool::new(false));
        let (search_channel_tx, search_channel_rx) = std::sync::mpsc::channel();

        // Spawn worker thread to handle searches in the background
        {
            let abort_flag = Arc::clone(&abort_flag);
            std::thread::spawn(move || worker_thread(abort_flag, search_channel_rx));
        }

        Self {
            search_channel_tx,
            abort_flag,
        }
    }

    pub fn signal_abort(&self) {
        self.abort_flag.store(true, Ordering::SeqCst);
    }

    pub fn queue_search(&self, project: Arc<Mutex<Project>>, ctx: egui::Context) {
        let params = project.lock().unwrap().clone_params();
        // Get the parameters now, in case they get changed while this query request is waiting in
        // the queue
        self.search_channel_tx.send((project, params, ctx)).unwrap();
    }
}

fn worker_thread(
    abort_flag: Arc<AtomicBool>,
    search_channel_rx: Receiver<(Arc<Mutex<Project>>, crate::Parameters, egui::Context)>,
) {
    while let Ok((project, params, ctx)) = search_channel_rx.recv() {
        println!("Got new query!");
        let project_guard = project.lock().unwrap();
        let search = monument::Search::new(params.to_monument(), monument::Config::default())
            .unwrap()
            .id_generator(project_guard.comp_id_generator());
        let comp_cache = project_guard.comp_cache();
        drop(project_guard); // Release the mutex before the search runs
        search.run(
            |update| {
                project.lock().unwrap().recieve_update(update);
                ctx.request_repaint();
            },
            &abort_flag,
        );
        println!("Finished query, waiting for next one");
    }
}
