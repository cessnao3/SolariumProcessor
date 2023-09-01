//use gtk::glib::clone;
use gtk::{Application, ApplicationWindow, Range, Scale, Adjustment, Label, EditableLabel};
use gtk::{glib, prelude::*};
use gtk::glib::clone;
use gtk::{Box, Button, Frame, ScrolledWindow, Stack, StackSwitcher, TextBuffer, TextView};

use crate::cpu_thread::cpu_thread;
use crate::messages::{ThreadToUi, UiToThread};

pub fn build_ui(app: &Application) {
    // Create the tx/rx for the secondary thread
    let (tx_ui, rx_thread) = std::sync::mpsc::channel::<UiToThread>();
    let (tx_thread, rx_ui) = glib::MainContext::channel::<ThreadToUi>(glib::Priority::DEFAULT);

    // Create a button with label and margins
    let button = Button::builder()
        .label("Press me!")
        .margin_top(12)
        .margin_bottom(12)
        .margin_start(12)
        .margin_end(12)
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
        .orientation(gtk::Orientation::Horizontal)
        .spacing(4)
        .margin_top(4)
        .margin_bottom(4)
        .margin_start(4)
        .margin_end(4)
        .build();
    let column_code = Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .spacing(4)
        .build();
    let column_cpu = Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .spacing(4)
        .build();

    column_code.append(&StackSwitcher::builder().stack(&code_stack).build());
    column_code.append(&code_stack);

    let cpu_controls_box = Box::builder().orientation(gtk::Orientation::Vertical).spacing(4).build();
    let cpu_controls_frame = Frame::builder().label("CPU Controls").child(&cpu_controls_box).build();

    let cpu_controls_buttons = Box::builder().orientation(gtk::Orientation::Horizontal).spacing(4).build();
    cpu_controls_box.append(&cpu_controls_buttons);

    let cpu_btns = vec![
        ("Step", UiToThread::CpuStep),
        ("Start", UiToThread::CpuStart),
        ("Stop", UiToThread::CpuStop),
        ("Reset", UiToThread::CpuReset),
        ("IRQ0", UiToThread::CpuIrq(0)),
    ];

    for (lbl, action) in cpu_btns.into_iter() {
        let btn = Button::builder().label(lbl).hexpand(true).build();
        let tx_ui_clone = tx_ui.clone();
        btn.connect_clicked(move |_| tx_ui_clone.send(action.clone()).unwrap());
        cpu_controls_buttons.append(&btn);
    }

    column_cpu.append(&cpu_controls_frame);

    let cpu_speed_scale = Scale::builder()
        .draw_value(true)
        .adjustment(&Adjustment::builder()
            .lower(1.0)
            .upper(10.0)
            .value(1.0)
            .step_increment(1.0)
            .build())
        .show_fill_level(true)
        .build();

    cpu_speed_scale.connect_move_slider(clone!(@strong tx_ui => move |sld, hdl| tx_ui.send(UiToThread::SetMultiplier(sld.value())).unwrap();));
    cpu_controls_box.append(&cpu_speed_scale);

    let register_box = Box::builder().orientation(gtk::Orientation::Horizontal).spacing(4).build();
    let register_box_a = Box::builder().orientation(gtk::Orientation::Vertical).spacing(4).build();
    let register_box_b = Box::builder().orientation(gtk::Orientation::Vertical).spacing(4).build();

    let register_frame = Frame::builder().label("Registers").child(&register_box).build();

    register_box.append(&register_box_a);
    register_box.append(&register_box_b);

    let mut register_fields = Vec::new();
    for i in 0..16 {
        let target_box = if i < 8 { register_box_a.clone() } else { register_box_b.clone() };

        let inner_box = Box::builder().orientation(gtk::Orientation::Horizontal).margin_start(4).margin_end(4).build();

        let label = Label::builder().label(&format!("R{i:02}")).margin_end(6).build();
        let text = EditableLabel::builder().text(&format!("0x{:04x}", 0)).hexpand(true).editable(false).build();

        inner_box.append(&label);
        inner_box.append(&text);

        register_fields.push(text);

        target_box.append(&inner_box);
    }

    column_cpu.append(&register_frame);

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

    column_cpu.append(&text_log_frame);

    columns.append(&column_code);
    columns.append(&column_cpu);

    // Create a window and set the title
    let window = ApplicationWindow::builder()
        .application(app)
        .title("V/SProc")
        .child(&columns)
        .build();

    // Setup the UI receiver
    rx_ui.attach(None, move |msg| {
        match msg {
            ThreadToUi::ThreadExit => (),
            _ => panic!("unknown message provided!")
        };

        glib::ControlFlow::Continue
    });

    window.connect_close_request(clone!(@strong tx_ui => move |w| {
        tx_ui.send(UiToThread::Exit).unwrap();
        glib::Propagation::Proceed
    }));

    // Create the accompanying thread
    std::thread::spawn(move || cpu_thread(rx_thread, tx_thread));

    // Present window
    window.present();
}
