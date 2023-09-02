//use gtk::glib::clone;
use gtk::glib::clone;
use gtk::{glib, prelude::*};
use gtk::{Adjustment, Application, ApplicationWindow, EditableLabel, Label, Scale};
use gtk::{Box, Button, Frame, ScrolledWindow, Stack, StackSwitcher, TextBuffer, TextView};

use crate::cpu_thread::cpu_thread;
use crate::messages::{ThreadToUi, UiToThread};

pub fn build_ui(app: &Application) {
    // Create the tx/rx for the secondary thread
    let (tx_ui, rx_thread) = std::sync::mpsc::channel::<UiToThread>();
    let (tx_thread, rx_ui) = glib::MainContext::channel::<ThreadToUi>(glib::Priority::DEFAULT);

    let columns = Box::builder()
        .orientation(gtk::Orientation::Horizontal)
        .spacing(4)
        .margin_top(4)
        .margin_bottom(4)
        .margin_start(4)
        .margin_end(4)
        .build();

    columns.append(&build_code_column(&tx_ui, &tx_thread));
    let (column_cpu, register_fields, buffer_log) = build_cpu_column(&tx_ui);
    columns.append(&column_cpu);
    columns.append(&build_serial_column(&tx_ui));

    // Create a window and set the title
    let window = ApplicationWindow::builder()
        .application(app)
        .title("V/SProc")
        .child(&columns)
        .build();

    // Setup the UI receiver
    rx_ui.attach(None, move |msg| {
        match msg {
            ThreadToUi::RegisterState(regs) => {
                for (i, r) in regs.iter().enumerate() {
                    register_fields[i].set_text(&format!("0x{:04x}", r.get()));
                }
            }
            ThreadToUi::LogMessage(msg) => {
                buffer_log.insert(&mut buffer_log.end_iter(), &format!("{msg}\n"));
            }
            ThreadToUi::ThreadExit => (),
            _ => panic!("unknown message provided!"),
        };

        glib::ControlFlow::Continue
    });

    window.connect_close_request(clone!(@strong tx_ui => move |_| {
        tx_ui.send(UiToThread::Exit).unwrap();
        glib::Propagation::Proceed
    }));

    // Create the accompanying thread
    std::thread::spawn(move || cpu_thread(rx_thread, tx_thread));

    // Present window
    window.present();
}

fn build_code_column(
    tx_ui: &std::sync::mpsc::Sender<UiToThread>,
    tx_thread: &gtk::glib::Sender<ThreadToUi>,
) -> Box {
    let code_stack = Stack::builder().build();

    let code_options = vec![
        ("; ASM Code", "Assemble", "ASM", true),
        ("// SPC Code", "Build", "SPC", false),
    ];

    for (comment, button_verb, short_name, is_assembly) in code_options.into_iter() {
        let buffer_assembly_code = TextBuffer::builder()
            .enable_undo(true)
            .text(comment)
            .build();

        let text_code = TextView::builder()
            .buffer(&buffer_assembly_code)
            .width_request(300)
            .height_request(400)
            .has_tooltip(true)
            .hexpand(true)
            .vexpand(true)
            .tooltip_text(&format!("Text input for {short_name} code"))
            .monospace(true)
            .build();
        let text_code_scroll = ScrolledWindow::builder().child(&text_code).build();
        let text_code_frame = Frame::builder()
            .label(&format!("{} Code Editor", short_name))
            .child(&text_code_scroll)
            .build();

        let code_box = Box::builder()
            .orientation(gtk::Orientation::Vertical)
            .spacing(4)
            .build();
        let btn_build = Button::builder().label(button_verb).build();

        if is_assembly {
            btn_build.connect_clicked(clone!(@strong tx_thread, @strong tx_ui => move |_| {
                let asm = buffer_assembly_code.text(&buffer_assembly_code.start_iter(), &buffer_assembly_code.end_iter(), false);
                match sda::assemble_text(asm.as_str()) {
                    Ok(v) => {
                        tx_ui.send(UiToThread::SetCode(v)).unwrap();
                        tx_thread.send(ThreadToUi::LogMessage(format!("{short_name} Successful"))).unwrap();
                    }
                    Err(e) => tx_thread.send(ThreadToUi::LogMessage(format!("{short_name}: {e}"))).unwrap(),
                }
            }));
        } else {
            btn_build.connect_clicked(clone!(@strong tx_thread => move |_| {
                tx_thread.send(ThreadToUi::LogMessage(format!("{short_name} compiling not supported"))).unwrap();
            }));
        }

        code_box.append(&text_code_frame);
        code_box.append(&btn_build);

        let page_asm = code_stack.add_child(&code_box);
        page_asm.set_title("ASM");
    }

    let column_code = Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .spacing(4)
        .build();

    column_code.append(&StackSwitcher::builder().stack(&code_stack).build());
    column_code.append(&code_stack);

    column_code
}

fn build_cpu_column(
    tx_ui: &std::sync::mpsc::Sender<UiToThread>,
) -> (Box, Vec<EditableLabel>, TextBuffer) {
    let column_cpu = Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .spacing(4)
        .build();

    let cpu_controls_box = Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .spacing(4)
        .build();
    let cpu_controls_frame = Frame::builder()
        .label("CPU Controls")
        .child(&cpu_controls_box)
        .build();

    let cpu_controls_buttons = Box::builder()
        .orientation(gtk::Orientation::Horizontal)
        .spacing(4)
        .build();
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
        btn.connect_clicked(clone!(@strong tx_ui => move |_| tx_ui.send(action.clone()).unwrap()));
        cpu_controls_buttons.append(&btn);
    }

    column_cpu.append(&cpu_controls_frame);

    let cpu_speed_scale = Scale::builder()
        .draw_value(true)
        .adjustment(
            &Adjustment::builder()
                .lower(1.0)
                .upper(10.0)
                .value(1.0)
                .build(),
        )
        .show_fill_level(true)
        .build();

    cpu_speed_scale.connect_change_value(clone!(@strong tx_ui => move |_, _, val| tx_ui.send(UiToThread::SetMultiplier(val)).unwrap(); glib::Propagation::Proceed));
    cpu_controls_box.append(&cpu_speed_scale);

    let register_box = Box::builder()
        .orientation(gtk::Orientation::Horizontal)
        .spacing(4)
        .build();
    let register_box_a = Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .spacing(4)
        .build();
    let register_box_b = Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .spacing(4)
        .build();

    let register_frame = Frame::builder()
        .label("Registers")
        .child(&register_box)
        .build();

    register_box.append(&register_box_a);
    register_box.append(&register_box_b);

    let mut register_fields = Vec::new();
    for i in 0..16 {
        let target_box = if i < 8 {
            register_box_a.clone()
        } else {
            register_box_b.clone()
        };

        let inner_box = Box::builder()
            .orientation(gtk::Orientation::Horizontal)
            .margin_start(4)
            .margin_end(4)
            .build();

        let label = Label::builder()
            .label(&format!("R{i:02}"))
            .margin_end(6)
            .build();
        let text = EditableLabel::builder()
            .text(&format!("0x{:04x}", 0))
            .hexpand(true)
            .editable(false)
            .build();

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

    (column_cpu, register_fields, buffer_log)
}

fn build_serial_column(tx_ui: &std::sync::mpsc::Sender<UiToThread>,) -> Box {
    let column_serial = Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .spacing(4)
        .build();

    let buffer_serial = TextBuffer::builder().build();

    let text_serial = TextView::builder()
        .buffer(&buffer_serial)
        .width_request(300)
        .height_request(200)
        .has_tooltip(true)
        .hexpand(true)
        .vexpand(true)
        .editable(false)
        .monospace(true)
        .tooltip_text("Serial log")
        .monospace(true)
        .build();
    let text_serial_frame = Frame::builder()
        .label("Serial Log")
        .child(&text_serial)
        .build();

    column_serial.append(&text_serial_frame);

    let text_input_box = Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .spacing(4)
        .build();

    let text_input_frame = Frame::builder()
        .label("Serial Input")
        .child(&text_input_box)
        .build();

    let text_input_buffer = TextBuffer::builder().build();

    let text_input = TextView::builder()
        .monospace(true)
        .buffer(&text_input_buffer)
        .build();

    let text_input_scroll = ScrolledWindow::builder().child(&text_input).build();
    text_input_box.append(&text_input_scroll);

    let text_input_button_box = Box::builder()
        .orientation(gtk::Orientation::Horizontal)
        .spacing(4)
        .build();
    let text_input_btn_submit = Button::builder().label("Submit").build();
    text_input_btn_submit.connect_clicked(clone!(@strong tx_ui => move |_| {
        let txt = text_input_buffer.text(&text_input_buffer.start_iter(), &text_input_buffer.end_iter(), false);
        tx_ui.send(UiToThread::SerialInput(txt.to_string())).unwrap();
        text_input_buffer.set_text("");
    }));

    text_input_button_box.append(&text_input_btn_submit);

    text_input_box.append(&text_input_button_box);

    column_serial.append(&text_input_frame);

    column_serial
}
