use std::{fmt::Display, hash::Hash};

use eframe::egui;
use monument::utils::TotalLength;

/// Helper type to create a table of parameters.
///
/// # Example
/// ```
/// ParamTable::show(ui, |table| {
///     table.add_param_widget("Name", egui::TextEdit::singleline(&mut self.name));
///     table.add_param("Age", |ui| {
///         ui.selectable_value(&mut self.is_adult, true, "Adult");
///         ui.selectable_value(&mut self.is_adult, false, "Child");
///     });
/// });
/// ```
/// produces a table like this (note how the values are all aligned):
///
///     Name: <self.name>
///     Age:  [Adult] <Child>
pub struct ParamTable<'ui> {
    ui: &'ui mut egui::Ui,
}

impl ParamTable<'_> {
    pub fn show(ui: &mut egui::Ui, id_source: impl Hash, f: impl FnOnce(&mut ParamTable<'_>)) {
        egui::Grid::new(id_source)
            .num_columns(2)
            .show(ui, |ui| f(&mut ParamTable { ui }));
    }

    pub fn add_param_widget(&mut self, label: &str, rhs: impl egui::Widget) {
        self.add_param(label, |ui| ui.add(rhs));
    }

    pub fn add_label(&mut self, label: &str, rhs: impl Display) {
        self.add_param_widget(label, egui::Label::new(rhs.to_string()));
    }

    pub fn add_todo(&mut self, label: &str) {
        let text = egui::RichText::new("TODO")
            .strong()
            .color(egui::Color32::RED);
        self.add_param_widget(label, egui::Label::new(text));
    }

    pub fn add_param<T>(&mut self, label: &str, rhs: impl FnOnce(&mut egui::Ui) -> T) {
        self.ui.label(format!("{label}:"));
        self.ui.scope(rhs);
        self.ui.end_row();
    }
}

pub fn len_range(min: usize, max: usize) -> std::ops::RangeInclusive<TotalLength> {
    TotalLength::new(min)..=TotalLength::new(max)
}
