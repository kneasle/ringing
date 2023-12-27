use std::sync::Arc;

use eframe::egui;
use itertools::Itertools;
use monument::{
    composition::{CompositionCache, CompositionId, ParamsData},
    utils::IdGenerator,
    Composition,
};
use ordered_float::OrderedFloat;

use crate::{search::SearchThreadHandle, utils::ParamTable};

/// A 'project' is a group of `Search`es, sharing methods, calls, etc.  Conceptually, these
/// searches are all iterations looking for the same compositions.
#[derive(Debug)]
pub struct Project {
    name: String,
    params: crate::Parameters,
    compositions: Vec<Composition>,

    pub comp_cache: CompositionCache,
    comp_id_generator: Arc<IdGenerator<CompositionId>>,
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
        let per_param_values = ParamsData::new(params);
        let mut cache_with_params = self.comp_cache.with_params(params);

        // Generate getters for all comps which still match the current params
        let mut comps_to_display = self
            .compositions
            .iter()
            .filter_map(|comp| comp.values_with_cache(&per_param_values, &mut cache_with_params))
            .collect_vec();
        // Sort them by total score
        comps_to_display.sort_by_cached_key(|values| OrderedFloat(-values.score_per_row()));
        // Display them in a grid
        // TODO: Custom (animated!) widget for this
        egui::Grid::new("Comp grid")
            .striped(true)
            .min_col_width(10.0)
            .show(ui, |ui| {
                /* Header */
                ui.strong("Length");
                ui.strong("|");
                ui.strong("Score");
                ui.strong("Score/row");

                ui.strong("|");
                ui.strong("Music");
                for (_idx, mt) in params.music_types_to_show() {
                    ui.strong(&mt.name);
                }

                ui.strong("|");
                ui.strong("Call string");
                ui.end_row();

                /* Compositions */
                for comp_values in comps_to_display {
                    ui.label(comp_values.length().to_string());

                    ui.label("|");
                    ui.label(format!("{:.2}", comp_values.total_score));
                    ui.label(format!("{:.6}", comp_values.score_per_row()));

                    ui.label("|");
                    ui.label(format!("{:.2}", comp_values.music_score));
                    for (idx, mt) in params.music_types_to_show() {
                        ui.label(mt.display_counts(comp_values.music_counts[idx], params.stage));
                    }

                    ui.label("|");
                    ui.label(&comp_values.call_string);
                    ui.end_row()
                }
            });
    }

    /////////////
    // HELPERS //
    /////////////

    pub fn comp_id_generator(&self) -> Arc<IdGenerator<CompositionId>> {
        self.comp_id_generator.clone()
    }

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

            comp_cache: CompositionCache::default(),
            comp_id_generator: Arc::new(IdGenerator::starting_at_zero()),
            search_progress: None,
        }
    }
}
