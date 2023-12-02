use eframe::egui::{self, RichText};
use itertools::Itertools;
use monument::{Composition, CompositionGetter};
use ordered_float::OrderedFloat;

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
        egui::ScrollArea::vertical().show(ui, |ui| self.params.draw_gui(ui));

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
                    self.draw_comps(&monument_params, ui);
                });
        }
    }

    fn draw_comps(&mut self, params: &monument::Parameters, ui: &mut egui::Ui) {
        // Generate getters for all comps which still match the current params
        let mut comp_getters = self
            .compositions
            .iter()
            .filter_map(|c| CompositionGetter::new(c, params))
            .collect_vec();
        // Sort them by total score
        comp_getters.sort_by_cached_key(|g| OrderedFloat(-g.total_score()));
        // Display them in a grid
        // TODO: Custom (animated!) widget for this
        egui::Grid::new("Comp grid").striped(true).show(ui, |ui| {
            // Header
            ui.label(RichText::new("Length").strong());
            ui.label(RichText::new("Score").strong());
            ui.label(RichText::new("Score/row").strong());
            ui.label(RichText::new("Call string").strong());
            ui.end_row();

            for g in comp_getters {
                ui.label(g.length().to_string());
                ui.label(format!("{:.2}", g.total_score()));
                ui.label(format!("{:.6}", g.score_per_row()));
                ui.label(g.call_string());
                ui.end_row()
            }
        });
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
