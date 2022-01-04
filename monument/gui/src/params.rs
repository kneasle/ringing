use std::{
    ops::RangeInclusive,
    sync::{Arc, Mutex},
    time::Instant,
};

use bellframe::{Bell, Mask, MethodLib, RowBuf, Stage, Stroke};
use eframe::egui::{self, Color32, Ui};
use itertools::Itertools;
use monument::{
    layout::{
        self,
        new::{Call, SpliceStyle},
        Layout,
    },
    Progress, Query,
};

use crate::{Search, Status};

const MAX_LENGTH: usize = 1_000_000;
const MAX_NUM_COMPS: usize = 10_000;

const LENGTH_PRESETS: [(&str, RangeInclusive<usize>); 4] = [
    ("Practice", 0..=300),
    ("QP", 1250..=1350),
    ("Half Peal", 2500..=2600),
    ("Peal", 5000..=5200),
];

/// Parameters for a composition search
#[derive(Debug, Clone)]
pub struct Params {
    /* GENERAL */
    min_length: usize,
    max_length: usize,
    num_comps: usize,
    lh_start: bool,
    snap_start: bool,
    lh_finish: bool,
    snap_finish: bool,
    allow_false: bool,
    /* METHODS */
    methods: Vec<Method>,
    splice_style: SpliceStyle,
    method_count_range: (), // RangeInclusive<usize>,
    /* COURSES */
    part_head: String,
    course_head_preset: CourseHeadPreset,
    custom_course_heads: Vec<(Mask, Bell)>,
    /* MUSIC */
    start_stroke: Stroke,
    music_types: Vec<()>,
    max_duffer_rows: Option<usize>,
}

#[derive(Debug, Clone)]
struct Method {
    title: String,
    shorthand: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CourseHeadPreset {
    TenorsTogether,
    Any,
    Custom,
}

impl CourseHeadPreset {
    fn to_masks(self, custom_masks: &[(Mask, Bell)], stage: Stage) -> Vec<(Mask, Bell)> {
        use layout::new::CourseHeadMaskPreset::*;
        let preset = match self {
            CourseHeadPreset::TenorsTogether => TenorsTogether,
            CourseHeadPreset::Any => SplitTenors,
            CourseHeadPreset::Custom => Custom(custom_masks.to_owned()),
        };
        preset.into_masks(stage)
    }
}

/////////
// GUI //
/////////

impl Params {
    /// Draw the parameter panel's GUI, returning `true` if the `Search!` button was clicked.
    pub fn draw_gui(&mut self, ui: &mut Ui, method_lib: &MethodLib) -> bool {
        let mut should_start_search = false;
        // Use bottom->top layout to make sure the 'Search' button is always at the bottom of the
        // screen
        ui.with_layout(
            egui::Layout::bottom_up(egui::Align::Center),
            |bottom_up_ui| {
                // Add the compose button
                bottom_up_ui.add_space(4.0);
                should_start_search = bottom_up_ui
                    .add(egui::Button::new("Search! ->").text_style(egui::TextStyle::Heading))
                    .clicked();
                bottom_up_ui.separator();
                // Add the rest of the GUI in a top->bottom layout
                bottom_up_ui.vertical(|scroll_ui| {
                    egui::ScrollArea::vertical().show(scroll_ui, |ui| {
                        draw_section(ui, true, "General", |ui| self.panel_general(ui));
                        draw_section(ui, false, "Methods", |ui| {
                            self.panel_methods(ui, method_lib)
                        });
                        draw_section(ui, false, "Courses", |ui| self.panel_courses(ui));
                        draw_section(ui, false, "Music", |ui| self.panel_music(ui));
                    });
                });
            },
        );
        should_start_search
    }

    fn panel_general(&mut self, ui: &mut Ui) {
        ui.label("Length:");
        ui.vertical(|ui| {
            ui.horizontal(|ui| {
                ui.add(
                    egui::DragValue::new(&mut self.min_length)
                        .speed(25)
                        .clamp_range(0..=self.max_length.min(MAX_LENGTH)),
                );
                ui.label("to");
                ui.add(
                    egui::DragValue::new(&mut self.max_length)
                        .speed(25)
                        .clamp_range(self.min_length..=MAX_LENGTH),
                );
                ui.label("(inclusive)");
            });
            ui.horizontal(|ui| {
                for (label, range) in LENGTH_PRESETS {
                    if ui.button(label).clicked() {
                        self.min_length = *range.start();
                        self.max_length = *range.end();
                    }
                }
            });
        });
        ui.end_row();

        /* NUM COMPS */
        ui.label("Num Comps:");
        ui.add(
            egui::DragValue::new(&mut self.num_comps)
                .speed(1)
                .clamp_range(0..=MAX_NUM_COMPS),
        );
        ui.end_row();

        ui.label("Starts/Finishes:");
        ui.label("TODO");
        ui.end_row();

        ui.label("Allow False:");
        ui.checkbox(&mut self.allow_false, "");
        ui.end_row();
    }

    fn panel_methods(&mut self, ui: &mut Ui, method_lib: &MethodLib) {
        ui.label("Splice Style:");
        ui.horizontal(|ui| {
            use SpliceStyle::*;
            ui.selectable_value(&mut self.splice_style, LeadLabels, "Leads");
            ui.selectable_value(&mut self.splice_style, CallLocations, "Call Locations");
            ui.selectable_value(&mut self.splice_style, Calls, "Calls");
        });
        ui.end_row();

        ui.label("Count Range:");
        ui.label("TODO");
        ui.end_row();

        ui.label("Methods:");
        ui.vertical(|ui| {
            for m in &mut self.methods {
                ui.text_edit_singleline(&mut m.title);
                if method_lib.get_by_title(&m.title).is_err() {
                    ui.colored_label(Color32::RED, "Method not found in CC library");
                }
            }
        });
        ui.end_row();
    }

    fn panel_courses(&mut self, ui: &mut Ui) {
        let stage = self.stage();
        ui.label("Part Head:");
        ui.vertical(|ui| {
            ui.add(
                egui::TextEdit::singleline(&mut self.part_head).hint_text(RowBuf::rounds(stage)),
            );
            let parsed_row = RowBuf::parse_with_stage(&self.part_head, stage);
            match &parsed_row {
                Ok(row) => {
                    let num_parts = row.closure().len();
                    ui.label(format!(
                        "{} part{}",
                        num_parts,
                        if num_parts == 1 { "" } else { "s" }
                    ))
                }
                Err(e) => ui.add(egui::Label::new(e).text_color(egui::Color32::RED)),
            };
        });
        ui.end_row();

        ui.label("Course Heads:");
        ui.horizontal(|ui| {
            use CourseHeadPreset::*;
            ui.selectable_value(
                &mut self.course_head_preset,
                TenorsTogether,
                "Tenors Together",
            );
            ui.selectable_value(&mut self.course_head_preset, Any, "Any");
            ui.selectable_value(&mut self.course_head_preset, Custom, "Custom...");
        });
        ui.end_row();
    }

    fn panel_music(&mut self, ui: &mut Ui) {
        ui.label("Music Types:");
        ui.label("TODO");
        ui.end_row();

        ui.label("Max Duffer Rows:");
        ui.label("TODO");
        ui.end_row();
    }

    fn stage(&self) -> Stage {
        Stage::MAJOR // TODO: Actually compute this
    }
}

impl Default for Params {
    fn default() -> Self {
        Self {
            min_length: 5000,
            max_length: 5200,
            num_comps: 30,

            lh_start: true,
            snap_start: false,
            lh_finish: true,
            snap_finish: true,

            allow_false: false,

            methods: vec![Method {
                title: "Yorkshire Surprise Major".to_owned(),
                shorthand: None,
            }],
            splice_style: SpliceStyle::LeadLabels,
            method_count_range: (),

            part_head: String::new(), // empty string parses to rounds on any stage
            course_head_preset: CourseHeadPreset::TenorsTogether,
            custom_course_heads: vec![],

            start_stroke: Stroke::Back, // The first lead head is at a backstroke
            music_types: vec![],
            max_duffer_rows: None,
        }
    }
}

fn draw_section(ui: &mut Ui, is_first: bool, title: &str, draw: impl FnOnce(&mut Ui)) {
    if !is_first {
        ui.add_space(20.0);
    }
    ui.heading(title);
    ui.add_space(6.0);
    egui::Grid::new(title).spacing([10.0, 8.0]).show(ui, draw);
}

/////////////////////////
// CONVERSION TO QUERY //
/////////////////////////

impl Params {
    pub(crate) fn to_search(&self, method_lib: &MethodLib) -> Result<Search, QueryGenError> {
        self.to_query(method_lib).map(Arc::new).map(|query| Search {
            query,
            mutex: Mutex::new(super::SearchMutex {
                comps: vec![],
                status: Status::InProgress {
                    start_time: Instant::now(),
                    progress: Progress::START,
                },
            }),
        })
    }

    pub(crate) fn to_query(&self, method_lib: &MethodLib) -> Result<Query, QueryGenError> {
        let stage = self.stage();
        let part_head = RowBuf::parse_with_stage(&self.part_head, stage)
            .map_err(QueryGenError::PartHeadParse)?;
        // + 1 because `max_length` is an inclusive bound
        let len_range = self.min_length..self.max_length + 1;

        let calls = Call::near_calls(stage).unwrap_or_default(); // TODO: Implement this properly
        let ch_masks = self
            .course_head_preset
            .to_masks(&self.custom_course_heads, stage);

        // Compute shorthands and lookup methods in the CC library
        let shorthands = monument::utils::default_shorthands(
            self.methods
                .iter()
                .map(|m| (m.title.as_str(), m.shorthand.as_deref())),
        );
        let mut methods = Vec::new();
        for (m, shorthand) in self.methods.iter().zip_eq(shorthands) {
            let mut method = method_lib
                .get_by_title(&m.title)
                .map_err(QueryGenError::MethodLib)?;
            method.set_label(0, Some(bellframe::method::LABEL_LEAD_END.to_owned()));
            methods.push(layout::new::Method::new(
                method,
                calls.clone(),
                ch_masks.clone(),
                shorthand,
                Some(&[0]), // Only allow normal starts
                None,       // Allow any finishes
            ));
        }

        let layout = Layout::new(
            methods,
            self.splice_style,
            &part_head,
            None, // Let Monument decide between lead-/course-wise
        )
        .map_err(QueryGenError::Layout)?;

        Ok(Query {
            layout,
            part_head,
            len_range: len_range.clone(),
            num_comps: self.num_comps,
            allow_false: self.allow_false,

            method_count_range: len_range, // TODO: Actually calculate this

            music_types: vec![],
            start_stroke: self.start_stroke,
            max_duffer_rows: self.max_duffer_rows,
            ch_weights: vec![],
        })
    }
}

#[derive(Debug)]
pub(crate) enum QueryGenError {
    PartHeadParse(bellframe::row::InvalidRowError),
    MethodLib(bellframe::method_lib::QueryError<()>),
    Layout(monument::layout::new::Error),
}
