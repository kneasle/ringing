use std::ops::RangeInclusive;

use bellframe::{Bell, InvalidRowError, Mask, MethodLib, RowBuf, Stage};
use eframe::egui::{self, Color32, Ui};
use monument::layout::new::SpliceStyle;

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
    part_head: RowEdit,
    course_head_preset: CourseHeadPreset,
    custom_course_heads: Vec<(Mask, Bell)>,
    /* MUSIC */
    music_types: Vec<()>,
    max_duffer_rows: Option<usize>,
}

#[derive(Debug, Clone)]
struct Method {
    title: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CourseHeadPreset {
    TenorsTogether,
    Any,
    Custom,
}

impl Params {
    pub fn draw_gui(&mut self, ui: &mut Ui, method_lib: &MethodLib) {
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

        // If compose was clicked, then do some composing
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
                egui::TextEdit::singleline(&mut self.part_head.box_contents)
                    .hint_text(RowBuf::rounds(stage)),
            );
            let parsed_row = RowBuf::parse_with_stage(&self.part_head.box_contents, stage);
            match &parsed_row {
                Ok(row) => ui.label(format!("{} parts", row.closure().len())),
                Err(e) => ui.add(egui::Label::new(e).text_color(egui::Color32::RED)),
            };
            self.part_head.row = parsed_row;
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
            }],
            splice_style: SpliceStyle::LeadLabels,
            method_count_range: (),

            part_head: RowEdit::rounds(Stage::MAJOR),
            course_head_preset: CourseHeadPreset::TenorsTogether,
            custom_course_heads: vec![],

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

#[derive(Debug, Clone)]
pub struct RowEdit {
    row: Result<RowBuf, InvalidRowError>,
    box_contents: String,
}

impl RowEdit {
    fn rounds(stage: Stage) -> Self {
        Self {
            row: Ok(RowBuf::rounds(stage)),
            box_contents: String::new(), // The empty string always parses to rounds
        }
    }
}
