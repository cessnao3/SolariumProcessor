use gtk::prelude::*;
use gtk::{glib, Application, ApplicationWindow};
use gtk::{TextView, TextBuffer, Frame, ScrolledWindow, Button, Box};

const APP_ID: &str = "com.orourke.Solarium.VisualSProc";

fn main() -> glib::ExitCode {
    // Create a new application
    let app = Application::builder().application_id(APP_ID).build();

    // Connect to "activate" signal of `app`
    app.connect_activate(build_ui);

    // Run the application
    app.run()
}

fn build_ui(app: &Application) {
    // Create a button with label and margins
    let button = Button::builder()
        .label("Press me!")
        .margin_top(12)
        .margin_bottom(12)
        .margin_start(12)
        .margin_end(12)
        .build();

    let buffer_code = std::rc::Rc::new(TextBuffer::builder()
        .enable_undo(true)
        .text("TESTING!")
        .build());

    let text_code = TextView::builder()
        .buffer(&*buffer_code)
        .width_request(500)
        .height_request(400)
        .has_tooltip(true)
        .hexpand(true)
        .vexpand(true)
        .tooltip_text("Text input for assembly code")
        .build();

    let text_code_scroll = ScrolledWindow::builder()
        .child(&text_code)
        .build();

    let text_code_frame = Frame::builder()
        .label("Code Editor")
        .child(&text_code_scroll)
        .build();

    text_code.set_monospace(true);
    text_code.set_hscroll_policy(gtk::ScrollablePolicy::Natural);
    text_code.set_vscroll_policy(gtk::ScrollablePolicy::Natural);

    // Connect to "clicked" signal of `button`
    button.connect_clicked(|button| {
        // Set the label to "Hello World!" after the button has been clicked on
        button.set_label("Hello World!");
    });

    let columns = Box::builder().vexpand(true).hexpand(true).orientation(gtk::Orientation::Horizontal).spacing(4).build();
    let column_1 = Box::new(gtk::Orientation::Vertical, 4);
    let column_2 = Box::new(gtk::Orientation::Vertical, 4);

    let btn1 = Button::builder().label("TESTING 1!").build();
    let btn2 = Button::builder().label("TESTING!").build();

    let btn1_buffer = buffer_code.clone();
    btn1.connect_clicked(move |_| println!("Button 1 -> {}", btn1_buffer.text(&btn1_buffer.start_iter(), &btn1_buffer.end_iter(), false).as_str()));
    let btn2_buffer = buffer_code.clone();
    btn2.connect_clicked(move |_| println!("Button 2 -> {}", btn2_buffer.text(&btn2_buffer.start_iter(), &btn2_buffer.end_iter(), false).as_str()));

    column_1.append(&text_code_frame);
    column_1.append(&button);
    column_2.append(&btn1);
    column_2.append(&btn2);

    columns.append(&column_1);
    columns.append(&column_2);

    // Create a window and set the title
    let window = ApplicationWindow::builder()
        .application(app)
        .title("My GTK App")
        .child(&columns)
        .build();

    // Present window
    window.present();
}
