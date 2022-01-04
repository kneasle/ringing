use eframe::egui;

pub fn centered_heading(ui: &mut egui::Ui, label: impl ToString) {
    ui.with_layout(egui::Layout::top_down(egui::Align::Center), |ui| {
        ui.heading(label)
    });
}
