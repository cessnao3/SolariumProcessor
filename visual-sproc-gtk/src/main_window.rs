//use gtk::glib::clone;
use gtk::{Application, ApplicationWindow};
use gtk::prelude::*;
use gtk::{Box, Button, Frame, ScrolledWindow, Stack, StackSwitcher, TextBuffer, TextView};

use std::sync::mpsc::channel;

use crate::cpu_thread::cpu_thread;
use crate::messages::{ThreadToUi, UiToThread};

pub fn build_ui(app: &Application) {
    // Create the tx/rx for the secondary thread
    let (tx_ui, rx_thread) = channel::<UiToThread>();
    let (tx_thread, rx_ui) = channel::<ThreadToUi>();

    // Create a button with label and margins
    let button = Button::builder()
        .label("Press me!")
        .margin_top(12)
        .margin_bottom(12)
        .margin_start(12)
        .margin_end(12)
        .build();

    let buffer_log = TextBuffer::builder().build();

    let text_log = TextView::builder()
        .buffer(&buffer_log)
        .width_request(300)
        .height_request(200)
        .has_tooltip(true)
        .hexpand(true)
        .vexpand(true)
        .editable(false)
        .tooltip_text("Application log")
        .monospace(true)
        .build();
    let text_log_frame = Frame::builder()
        .label("Application Log")
        .child(&text_log)
        .build();

    let code_stack = Stack::builder().build();

    let buffer_assembly_code = TextBuffer::builder()
        .enable_undo(true)
        .text("; ASM Code")
        .build();
    {
        let text_code = TextView::builder()
            .buffer(&buffer_assembly_code)
            .width_request(300)
            .height_request(400)
            .has_tooltip(true)
            .hexpand(true)
            .vexpand(true)
            .tooltip_text("Text input for assembly code")
            .monospace(true)
            .build();
        let text_code_scroll = ScrolledWindow::builder().child(&text_code).build();
        let text_code_frame = Frame::builder()
            .label("Assembly Code Editor")
            .child(&text_code_scroll)
            .build();

        let code_box = Box::builder()
            .orientation(gtk::Orientation::Vertical)
            .spacing(4)
            .build();
        let btn_build = Button::builder().label("Assemble").build();

        code_box.append(&text_code_frame);
        code_box.append(&btn_build);

        let page_asm = code_stack.add_child(&code_box);
        page_asm.set_title("ASM");
    }

    let buffer_spc_code = TextBuffer::builder()
        .enable_undo(true)
        .text("// SPC Code")
        .build();
    {
        let text_code = TextView::builder()
            .buffer(&buffer_spc_code)
            .width_request(300)
            .height_request(400)
            .has_tooltip(true)
            .hexpand(true)
            .vexpand(true)
            .tooltip_text("Text input for SPC code")
            .monospace(true)
            .build();
        let text_code_scroll = ScrolledWindow::builder().child(&text_code).build();
        let text_code_frame = Frame::builder()
            .label("SPC Code Editor")
            .child(&text_code_scroll)
            .build();

        let code_box = Box::builder()
            .orientation(gtk::Orientation::Vertical)
            .spacing(4)
            .build();
        let btn_build = Button::builder().label("Compile").build();

        code_box.append(&text_code_frame);
        code_box.append(&btn_build);

        let page_spc = code_stack.add_child(&code_box);
        page_spc.set_title("SPC");
    }

    // Connect to "clicked" signal of `button`
    button.connect_clicked(|button| {
        // Set the label to "Hello World!" after the button has been clicked on
        button.set_label("Hello World!");
    });

    let columns = Box::builder()
        .vexpand(true)
        .hexpand(true)
        .orientation(gtk::Orientation::Horizontal)
        .spacing(4)
        .margin_top(4)
        .margin_bottom(4)
        .margin_start(4)
        .margin_end(4)
        .build();
    let column_code = Box::builder()
        .vexpand(true)
        .hexpand(true)
        .orientation(gtk::Orientation::Vertical)
        .spacing(4)
        .build();
    let column_2 = Box::builder()
        .vexpand(true)
        .hexpand(true)
        .orientation(gtk::Orientation::Vertical)
        .spacing(4)
        .build();

    let btn1 = Button::builder().label("TESTING 1!").build();
    let btn2 = Button::builder().label("TESTING!").build();

    //btn1.connect_clicked(clone!(@weak buffer_code => move |b| b.set_label(&format!("B1: {}", buffer_code.text(&buffer_code.start_iter(), &buffer_code.end_iter(), false).as_str()))));
    //btn2.connect_clicked(clone!(@weak buffer_code => move |b| b.set_label(&format!("B2: {}", buffer_code.text(&buffer_code.start_iter(), &buffer_code.end_iter(), false).as_str()))));

    column_code.append(&StackSwitcher::builder().stack(&code_stack).build());
    column_code.append(&code_stack);
    column_2.append(&btn1);
    column_2.append(&btn2);
    column_2.append(&text_log_frame);

    columns.append(&column_code);
    columns.append(&column_2);

    // Create a window and set the title
    let window = ApplicationWindow::builder()
        .application(app)
        .title("V/SProc")
        .child(&columns)
        .build();

    // Create the accompanying thread
    std::thread::spawn(move || cpu_thread(rx_thread, tx_thread));

    // Present window
    window.present();
}
