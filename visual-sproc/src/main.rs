mod cpu_thread;
mod main_window;
mod messages;

//use gtk::glib::clone;
use gtk::prelude::*;
use gtk::{glib, Application};

const APP_ID: &str = "com.orourke.Solarium.VSProc";

fn main() -> glib::ExitCode {
    // Create a new application
    let app = Application::builder().application_id(APP_ID).build();

    // Connect to "activate" signal of `app`
    app.connect_activate(main_window::build_ui);

    // Run the application
    app.run()
}
