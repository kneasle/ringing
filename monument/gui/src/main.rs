mod parameters;
mod params_gui;
mod project;
mod search;
mod utils;

use std::sync::{Arc, Mutex};

use eframe::egui;
use parameters::Parameters;
use project::Project;
use search::SearchThreadHandle;
use simple_logger::SimpleLogger;

const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    SimpleLogger::new()
        .without_timestamps()
        .with_colors(true)
        .with_level(log::LevelFilter::Info)
        .init()
        .unwrap();

    eframe::run_native(
        &format!("Monument v{}", VERSION),
        eframe::NativeOptions::default(),
        Box::new(MonumentApp::new),
    );
}

/// Singleton struct which holds all the data for one running instance of Monument
struct MonumentApp {
    projects: Vec<Arc<Mutex<Project>>>,
    search_handle: SearchThreadHandle,
}

impl MonumentApp {
    #[allow(clippy::new_ret_no_self)]
    fn new(_cc: &eframe::CreationContext<'_>) -> Box<dyn eframe::App> {
        Box::new(Self {
            projects: vec![Arc::new(Mutex::new(Project::default()))],
            search_handle: SearchThreadHandle::new(),
        })
    }
}

impl eframe::App for MonumentApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        let current_project_mutex: &Arc<_> = &self.projects[0];
        let mut current_project = current_project_mutex.lock().unwrap();

        let should_begin_search = egui::SidePanel::left("Params panel")
            .default_width(400.0)
            .show(ctx, |ui| {
                current_project.draw_left_panel(ui, &self.search_handle)
            })
            .inner;

        egui::SidePanel::right("Comps panel")
            .default_width(300.0)
            .show(ctx, |ui| {
                ui.heading("Composition display");
            });

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Compositions");
            current_project.draw_composition_list(ui);
        });

        if should_begin_search {
            drop(current_project); // Release the mutex before `search_handle` acquires it again
            self.search_handle
                .queue_search(current_project_mutex.clone(), ctx.clone());
        }
    }
}
