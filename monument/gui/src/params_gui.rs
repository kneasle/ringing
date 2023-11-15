use bellframe::Stroke;
use eframe::egui;
use monument::parameters::{default_shorthand, Call, SpliceStyle};

use crate::utils::{len_range, ParamTable};

pub fn draw_params_gui(params: &mut crate::Parameters, ui: &mut egui::Ui) {
    #[allow(clippy::type_complexity)]
    let sections: [(&str, fn(&mut crate::Parameters, &mut egui::Ui)); 5] = [
        ("General", draw_general_params),
        (
            &format!(
                "Methods ({}/{})",
                params.used_methods().count(),
                params.maybe_unused_methods.len()
            ),
            draw_method_params,
        ),
        (
            &format!(
                "Calls ({}/{})",
                params.used_calls().count(),
                params.maybe_unused_calls.len()
            ),
            draw_calls_params,
        ),
        ("Music", draw_music_params),
        ("Courses", draw_courses_params),
    ];

    let mut is_first = true;
    for (idx, (name, draw_fn)) in sections.into_iter().enumerate() {
        if !is_first {
            ui.add_space(30.0);
        }
        is_first = false;
        egui::CollapsingHeader::new(name)
            .id_source(idx) // Use index as an ID source because header names may change
            .default_open(true)
            .show(ui, |ui| draw_fn(params, ui));
    }
}

fn draw_general_params(params: &mut crate::Parameters, ui: &mut egui::Ui) {
    let params = &mut params.inner;
    ParamTable::show(ui, 0, |grid| {
        grid.add_label("Stage", params.stage);
        grid.add_param("Length", |ui| {
            ui.vertical(|ui| {
                let mut min = params.length.start().as_usize();
                let mut max = params.length.end().as_usize();
                let mut set_length = |min: usize, max: usize| {
                    params.length = len_range(min, max);
                };

                ui.horizontal(|ui| {
                    ui.add(egui::DragValue::new(&mut min));
                    ui.label("to");
                    ui.add(egui::DragValue::new(&mut max));
                    ui.label("(inclusive)");
                    set_length(min, max);
                });
                ui.horizontal(|ui| {
                    if ui.button("Practice").clicked() {
                        set_length(0, 300);
                    }
                    if ui.button("QP").clicked() {
                        set_length(1250, 1350);
                    }
                    if ui.button("Half Peal").clicked() {
                        set_length(2500, 2600);
                    }
                    if ui.button("Peal").clicked() {
                        set_length(5000, 5200);
                    }
                });
            });
        });
        grid.add_param_widget("Num comps", egui::DragValue::new(&mut params.num_comps));
        grid.add_param("Truth", |ui| {
            ui.selectable_value(&mut params.require_truth, true, "Require truth");
            ui.selectable_value(&mut params.require_truth, false, "Allow falseness");
        });
    });
}

fn draw_method_params(params: &mut crate::Parameters, ui: &mut egui::Ui) {
    // Other params
    for (used, m) in &mut params.maybe_unused_methods {
        draw_method_gui(used, m, ui);
    }
    ui.separator();
    if !params.is_spliced() {
        ui.label("(Include more than one method to show splicing options)");
    } else {
        ParamTable::show(ui, 0, |grid| {
            grid.add_param("Splice locations", |ui| {
                ui.selectable_value(
                    &mut params.inner.splice_style,
                    SpliceStyle::LeadLabels,
                    "At leads",
                );
                ui.selectable_value(
                    &mut params.inner.splice_style,
                    SpliceStyle::Calls,
                    "At calls",
                );
            });
            grid.add_param_widget(
                "Score per splice",
                egui::Slider::new(&mut params.inner.splice_weight, -5.0..=5.0),
            );
            grid.add_todo("Count range");
            grid.add_todo("Start indices");
            grid.add_todo("End indices");
        });
    }
}

fn draw_method_gui(used: &mut bool, method: &mut monument::parameters::Method, ui: &mut egui::Ui) {
    let heading = format!("{}: {}", method.shorthand(), method.title());
    egui::CollapsingHeader::new(heading)
        .id_source(method.id)
        .show(ui, |ui| {
            ParamTable::show(ui, 0, |table| {
                table.add_param_widget("Name", egui::TextEdit::singleline(&mut method.inner.name));
                let default_shorthand = default_shorthand(&method.title());
                table.add_param_widget(
                    "Shorthand",
                    egui::TextEdit::singleline(&mut method.custom_shorthand)
                        .hint_text(default_shorthand),
                );
                table.add_param_widget("Used", egui::Checkbox::new(used, ""));
            });
            ui.separator();
            ParamTable::show(ui, 1, |table| {
                table.add_label("Class", method.inner.class());
                table.add_label("Stage", method.inner.stage());
            });
        });
}

fn draw_calls_params(params: &mut crate::Parameters, ui: &mut egui::Ui) {
    for (used, c) in &mut params.maybe_unused_calls {
        draw_call_ui(used, c, ui);
    }
    /*
    TODO: Display call params
    ui.separator();
    ParamTable::show(ui, 0, |grid| {
        grid.add_param("Calling bell", |ui| {
            for b in params.stage.bells() {
                ui.selectable_value(&mut params.calling_bell, b, b.name());
            }
        });
    });
    */
}

fn draw_call_ui(used: &mut bool, call: &mut Call, ui: &mut egui::Ui) {
    let label_str = if call.label_from == call.label_to {
        call.label_from.clone()
    } else {
        format!("{}->{}", call.label_from, call.label_to)
    };
    let heading = format!("{} @ {} ({})", call.symbol, label_str, call.place_notation);

    egui::CollapsingHeader::new(heading)
        .id_source(call.id)
        .show(ui, |ui| {
            ParamTable::show(ui, 0, |table| {
                table.add_param_widget("Symbol", egui::TextEdit::singleline(&mut call.symbol));
                table.add_param_widget("Used", egui::Checkbox::new(used, ""));
                table.add_param_widget("Weight", egui::Slider::new(&mut call.weight, -20.0..=2.0));
                table.add_label("Place notation", &call.place_notation);
                table.add_label("Lead location (from)", &call.label_from);
                table.add_label("Lead location (to)", &call.label_to);
                table.add_label("Calling positions", call.calling_positions.join(""));
            });
        });
}

fn draw_music_params(params: &mut crate::Parameters, ui: &mut egui::Ui) {
    ParamTable::show(ui, 0, |grid| {
        grid.add_todo("Music types");
        grid.add_param("Start stroke", |ui| {
            ui.selectable_value(&mut params.inner.start_stroke, Stroke::Hand, "Handstroke");
            ui.selectable_value(&mut params.inner.start_stroke, Stroke::Back, "Backstroke");
        });
    });
}

fn draw_courses_params(_params: &mut crate::Parameters, ui: &mut egui::Ui) {
    ParamTable::show(ui, 0, |grid| {
        grid.add_todo("Start row");
        grid.add_todo("End row");
        grid.add_todo("Part head");
        grid.add_todo("Allowed courses");
        grid.add_todo("Course weights");
    });
}
