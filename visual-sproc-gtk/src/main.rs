use gtk::glib::clone;
use gtk::{prelude::*, StackSwitcher, Stack};
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

    let buffer_code = TextBuffer::builder()
        .enable_undo(true)
        .text("TESTING!")
        .build();

    let text_code = TextView::builder()
        .buffer(&buffer_code)
        .width_request(500)
        .height_request(400)
        .has_tooltip(true)
        .hexpand(true)
        .vexpand(true)
        .tooltip_text("Text input for assembly code")
        .monospace(true)
        .build();

    let text_code_scroll = ScrolledWindow::builder()
        .child(&text_code)
        .build();

    let text_code_frame = Frame::builder()
        .label("Code Editor")
        .child(&text_code_scroll)
        .build();

    let code_stack = Stack::builder().build();
    let p1 = code_stack.add_child(&text_code_frame);
    p1.set_title("Name!");
    let p2 = code_stack.add_child(&Button::builder().label("TESTING!").build());
    p2.set_title("NAME2");
    let code_switcher = StackSwitcher::builder().stack(&code_stack).build();

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

    btn1.connect_clicked(clone!(@weak buffer_code => move |b| b.set_label(&format!("B1: {}", buffer_code.text(&buffer_code.start_iter(), &buffer_code.end_iter(), false).as_str()))));
    btn2.connect_clicked(clone!(@weak buffer_code => move |b| b.set_label(&format!("B2: {}", buffer_code.text(&buffer_code.start_iter(), &buffer_code.end_iter(), false).as_str()))));

    column_1.append(&code_switcher);
    column_1.append(&code_stack);
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
