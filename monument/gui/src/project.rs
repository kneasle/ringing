use eframe::egui;
use monument::{Composition, CompositionGetter};

use crate::{search::SearchThreadHandle, utils::ParamTable};

/// A 'project' is a group of `Search`es, sharing methods, calls, etc.  Conceptually, these
/// searches are all iterations looking for the same compositions.
#[derive(Debug)]
pub struct Project {
    name: String,
    params: crate::Parameters,
    compositions: Vec<Composition>,
    search_progress: Option<monument::Progress>,
}

impl Project {
    /////////
    // GUI //
    /////////

    #[must_use]
    pub fn draw_left_panel(
        &mut self,
        ui: &mut egui::Ui,
        search_handle: &SearchThreadHandle,
    ) -> bool {
        // Header section
        ui.heading(format!("Monument v{}", crate::VERSION));
        ui.add_space(30.0);
        self.draw_header_ui(ui);
        ui.separator();

        // Search section
        let should_search = self.draw_search_ui(ui, search_handle);
        ui.separator();

        // Comp params
        ui.add_space(10.0);
        ui.heading("Composition Parameters");
        egui::ScrollArea::vertical().show(ui, |ui| {
            crate::params_gui::draw_params_gui(&mut self.params, ui)
        });

        should_search
    }

    pub fn draw_header_ui(&mut self, ui: &mut egui::Ui) {
        ui.horizontal(|ui| {
            ui.label("Project name: ");
            ui.text_edit_singleline(&mut self.name);
        });
    }

    #[must_use]
    pub fn draw_search_ui(
        &mut self,
        ui: &mut egui::Ui,
        search_handle: &SearchThreadHandle,
    ) -> bool {
        if let Some(progress) = &self.search_progress {
            ParamTable::show(ui, 0, |table| {
                table.add_label("Iterations", progress.iter_count);
                table.add_label("Compositions generated", progress.num_comps); // TODO: Num unique
                table.add_label("Prefixes in queue", progress.queue_len); // TODO: Truncating
                table.add_label("Mean prefix length", progress.avg_length);
                table.add_label("Max prefix length", progress.max_length);
            });
            if progress.truncating_queue {
                ui.label("Truncating queue...");
            }
            if progress.aborting {
                ui.label("Aborting...");
            } else if ui.button("Abort search").clicked() {
                search_handle.signal_abort();
            }
            false
        } else {
            // Search section.  It's either a button (if no search is running) or an update display
            // (if a search is running)
            let generate_text = if self.has_comps() {
                "Generate better compositions"
            } else {
                "Generate compositions"
            };
            ui.vertical_centered(|ui| ui.button(generate_text).clicked())
                .inner
        }
    }

    pub fn draw_composition_list(&mut self, ui: &mut egui::Ui) {
        let monument_params = self.params.to_monument();

        if !self.has_comps() {
            ui.vertical_centered(|ui| ui.label("No compositions yet"));
        } else {
            egui::ScrollArea::vertical()
                .auto_shrink([false, false])
                .show(ui, |ui| {
                    for c in &self.compositions {
                        if let Some(getter) = CompositionGetter::new(c, &monument_params) {
                            ui.label(getter.call_string());
                        }
                    }
                });
        }
    }

    /////////////
    // HELPERS //
    /////////////

    pub fn clone_params(&self) -> crate::Parameters {
        self.params.clone()
    }

    pub fn recieve_update(&mut self, update: monument::Update) {
        match update {
            monument::Update::Comp(comp) => self.compositions.push(comp),
            monument::Update::Progress(progress) => self.search_progress = Some(progress),
            monument::Update::Complete => self.search_progress = None,
        }
    }

    pub fn has_comps(&self) -> bool {
        !self.compositions.is_empty()
    }
}

impl Default for Project {
    fn default() -> Self {
        Self {
            name: "Yorksire S8 Peals".to_owned(),
            params: crate::Parameters::yorkshire_s8_qps(),
            compositions: vec![],
            search_progress: None,
        }
    }
}
