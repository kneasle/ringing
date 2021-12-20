mod params;

use std::sync::{mpsc::Receiver, Arc, Mutex};

use bellframe::MethodLib;
use eframe::{
    egui::{self, panel::Side, CtxRef},
    epi,
};
use monument::Query;
use params::Params;

fn main() {
    eframe::run_native(
        Box::new(MonumentApp::default()),
        eframe::NativeOptions::default(),
    );
}

/// Singleton struct containing all data required to run a single session of Monument's GUI
struct MonumentApp {
    params: Params,
    cc_method_lib: MethodLib,
    comps: Arc<Mutex<Vec<Comp>>>,
}

impl epi::App for MonumentApp {
    fn name(&self) -> &str {
        "Monument"
    }

    fn update(&mut self, ctx: &CtxRef, _: &mut epi::Frame<'_>) {
        egui::SidePanel::new(Side::Left, "params_panel").show(ctx, |ui| {
            self.params.draw_gui(ui, &self.cc_method_lib);
        });
        egui::SidePanel::new(Side::Right, "view_panel").show(ctx, |ui| {
            ui.heading("Comp View");
        });
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Comps");
        });
    }
}

impl Default for MonumentApp {
    fn default() -> Self {
        Self {
            params: Params::default(),
            cc_method_lib: MethodLib::cc_lib().expect("couldn't load CC methods"),
            comps: Arc::new(Mutex::new(vec![])),
        }
    }
}

struct Comp {
    params: Arc<Params>,
    inner: monument::Comp,
}

fn compose_thread(
    request_channel: Receiver<(CtxRef, Params, Query)>,
    comp_vec: Arc<Mutex<Vec<Comp>>>,
) {
    let mut ctx: Option<CtxRef> = None;
}
