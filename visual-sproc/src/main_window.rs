//use gtk::glib::clone;
use crate::cpu_thread::cpu_thread;
use crate::messages::{ThreadToUi, UiToThread};
use gtk::glib::clone;
use gtk::{glib, prelude::*};
use gtk::{Application, ApplicationWindow};

pub fn build_ui(app: &Application) {
    // Create the tx/rx for the secondary thread
    let (tx_ui, rx_thread) = std::sync::mpsc::channel::<UiToThread>();
    let (tx_thread, rx_ui) = glib::MainContext::channel::<ThreadToUi>(glib::Priority::DEFAULT);

    let columns = gtk::Box::builder()
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
    let (column_serial, memory_vals, buffer_serial) = build_serial_column(&tx_ui, &tx_thread);
    columns.append(&column_serial);

    // Create a window and set the title
    let window = ApplicationWindow::builder()
        .application(app)
        .title("V/SProc")
        .child(&columns)
        .build();

    // Setup the UI receiver
    rx_ui.attach(None, move |msg| {
        match msg {
            ThreadToUi::ProcessorReset => {
                buffer_serial.set_text("");
            }
            ThreadToUi::RegisterState(regs) => {
                for (i, r) in regs.iter().enumerate() {
                    register_fields[i].set_text(&format!("0x{:04x}", r.get()));
                }
            }
            ThreadToUi::LogMessage(msg) => {
                buffer_log.insert(&mut buffer_log.end_iter(), &format!("{msg}\n"));
            }
            ThreadToUi::SerialOutput(msg) => {
                buffer_serial.insert(&mut buffer_serial.end_iter(), msg.as_str());
            }
            ThreadToUi::ResponseMemory(base, vals) => {
                for (i, l) in memory_vals.labels.iter().enumerate() {
                    l.set_text(&format!("L{:04x}", base + 4 * i));
                }

                for (i, m) in memory_vals.locations.iter().enumerate() {
                    if i < vals.len() {
                        m.set_text(&format!("{:04x}", vals[i].get()));
                    } else {
                        m.set_text("");
                    }
                }
            }
            ThreadToUi::ThreadExit => return glib::ControlFlow::Break,
        };

        glib::ControlFlow::Continue
    });

    window.connect_close_request(clone!(@strong tx_ui => move |_| {
        tx_ui.send(UiToThread::Exit).unwrap();
        glib::Propagation::Proceed
    }));

    // Create the accompanying thread
    std::thread::spawn(move || cpu_thread(rx_thread, tx_thread));

    // Activate the memory
    memory_vals.base_input.unwrap().emit_activate();

    // Present window
    window.present();
}

fn build_code_column(
    tx_ui: &std::sync::mpsc::Sender<UiToThread>,
    tx_thread: &gtk::glib::Sender<ThreadToUi>,
) -> gtk::Box {
    let code_stack = gtk::Stack::builder().build();

    let default_asm = include_str!("../../examples/spa/serial_echo.smc");

    let code_options = vec![
        (default_asm, "Assemble", "ASM", true),
        ("// SPC Code", "Build", "SPC", false),
    ];

    for (comment, button_verb, short_name, is_assembly) in code_options.into_iter() {
        let buffer_assembly_code = gtk::TextBuffer::builder()
            .enable_undo(true)
            .text(comment)
            .build();

        let text_code = gtk::TextView::builder()
            .buffer(&buffer_assembly_code)
            .width_request(300)
            .height_request(400)
            .has_tooltip(true)
            .hexpand(true)
            .vexpand(true)
            .tooltip_text(&format!("Text input for {short_name} code"))
            .monospace(true)
            .build();
        let text_code_scroll = gtk::ScrolledWindow::builder().child(&text_code).build();
        let text_code_frame = gtk::Frame::builder()
            .label(&format!("{} Code Editor", short_name))
            .child(&text_code_scroll)
            .build();

        let code_box = gtk::Box::builder()
            .orientation(gtk::Orientation::Vertical)
            .spacing(4)
            .build();
        let btn_build = gtk::Button::builder().label(button_verb).build();

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

        let page = code_stack.add_child(&code_box);
        page.set_title(short_name);
    }

    let column_code = gtk::Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .spacing(4)
        .build();

    column_code.append(&gtk::StackSwitcher::builder().stack(&code_stack).build());
    column_code.append(&code_stack);

    column_code
}

fn build_cpu_column(
    tx_ui: &std::sync::mpsc::Sender<UiToThread>,
) -> (gtk::Box, Vec<gtk::EditableLabel>, gtk::TextBuffer) {
    let column_cpu = gtk::Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .spacing(4)
        .build();

    let cpu_controls_box = gtk::Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .spacing(4)
        .build();
    let cpu_controls_frame = gtk::Frame::builder()
        .label("CPU Controls")
        .child(&cpu_controls_box)
        .build();

    let cpu_controls_buttons = gtk::Box::builder()
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
        let btn = gtk::Button::builder().label(lbl).hexpand(true).build();
        btn.connect_clicked(clone!(@strong tx_ui => move |_| tx_ui.send(action.clone()).unwrap()));
        cpu_controls_buttons.append(&btn);
    }

    column_cpu.append(&cpu_controls_frame);

    let cpu_speed_scale = gtk::Scale::builder()
        .draw_value(true)
        .adjustment(
            &gtk::Adjustment::builder()
                .lower(1.0)
                .upper(10.0)
                .value(1.0)
                .build(),
        )
        .show_fill_level(true)
        .build();

    cpu_speed_scale.connect_change_value(clone!(@strong tx_ui => move |_, _, val| {
        tx_ui.send(UiToThread::SetMultiplier(val)).unwrap();
        glib::Propagation::Proceed
    }));
    cpu_controls_box.append(&cpu_speed_scale);

    let register_box = gtk::Box::builder()
        .orientation(gtk::Orientation::Horizontal)
        .spacing(4)
        .build();
    let register_box_a = gtk::Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .spacing(4)
        .build();
    let register_box_b = gtk::Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .spacing(4)
        .build();

    let register_frame = gtk::Frame::builder()
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

        let inner_box = gtk::Box::builder()
            .orientation(gtk::Orientation::Horizontal)
            .margin_start(4)
            .margin_end(4)
            .build();

        let label = gtk::Label::builder()
            .label(&format!("R{i:02}"))
            .margin_end(6)
            .build();
        let text = gtk::EditableLabel::builder()
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

    let buffer_log = gtk::TextBuffer::builder().build();

    let text_log = gtk::TextView::builder()
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
    let text_scroll = gtk::ScrolledWindow::builder().child(&text_log).build();
    let text_log_frame = gtk::Frame::builder()
        .label("Application Log")
        .child(&text_scroll)
        .build();

    column_cpu.append(&text_log_frame);

    (column_cpu, register_fields, buffer_log)
}

struct MemoryLocationData {
    labels: Vec<gtk::Label>,
    locations: Vec<gtk::Label>,
    base_input: Option<gtk::Entry>,
}

impl MemoryLocationData {
    fn new() -> Self {
        Self {
            labels: Vec::new(),
            locations: Vec::new(),
            base_input: None,
        }
    }
}

fn build_serial_column(
    tx_ui: &std::sync::mpsc::Sender<UiToThread>,
    tx_thread: &gtk::glib::Sender<ThreadToUi>,
) -> (gtk::Box, MemoryLocationData, gtk::TextBuffer) {
    let column_serial = gtk::Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .spacing(4)
        .build();

    let memory_box = gtk::Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .build();
    let memory_frame = gtk::Frame::builder()
        .label("Memory")
        .child(&memory_box)
        .build();

    let mut memory = MemoryLocationData::new();

    let memory_base_entry = gtk::Entry::builder().text("0").build();

    let memory_grid = gtk::Grid::builder()
        .hexpand(true)
        .row_spacing(4)
        .column_homogeneous(true)
        .build();
    for i in 0..6 {
        let label = gtk::Label::builder().label(format!("R{i}")).build();
        memory_grid.attach(&label, 0, i, 1, 1);
        memory.labels.push(label);

        for j in 0..4 {
            let mem_lbl = gtk::Label::builder()
                .label(format!("0x{:04x}", i * 4 + j))
                .build();
            memory_grid.attach(&mem_lbl, 1 + j, i, 1, 1);
            memory.locations.push(mem_lbl);
        }
    }

    let memory_count = memory.locations.len();
    memory_base_entry.connect_activate(clone!(@strong tx_ui, @strong tx_thread => move |t| {
        match usize::from_str_radix(&t.text(), 16) {
            Ok(v) => tx_ui.send(UiToThread::RequestMemory(v, memory_count)).unwrap(),
            Err(_) => {
                tx_thread.send(ThreadToUi::LogMessage(format!("Unable to set '{}' as base in hex", t.text()))).unwrap();
            }
        }
    }));

    memory_box.append(&memory_base_entry);
    memory_box.append(&memory_grid);
    column_serial.append(&memory_frame);

    memory.base_input = Some(memory_base_entry);

    let buffer_serial = gtk::TextBuffer::builder().build();

    let text_serial = gtk::TextView::builder()
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
    let text_serial_scroll = gtk::ScrolledWindow::builder().child(&text_serial).build();
    let text_serial_frame = gtk::Frame::builder()
        .label("Serial Log")
        .child(&text_serial_scroll)
        .build();

    column_serial.append(&text_serial_frame);

    let text_input_box = gtk::Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .spacing(4)
        .build();

    let text_input_frame = gtk::Frame::builder()
        .label("Serial Input")
        .child(&text_input_box)
        .build();

    let text_input = gtk::Entry::builder().build();
    text_input.connect_activate(clone!(@strong tx_ui => move |t| {
        tx_ui.send(UiToThread::SerialInput(t.text().to_string())).unwrap();
        t.set_text("");
    }));

    text_input_box.append(&text_input);

    let text_input_button_box = gtk::Box::builder()
        .orientation(gtk::Orientation::Horizontal)
        .spacing(4)
        .build();
    let text_input_btn_submit = gtk::Button::builder().label("Submit").build();
    text_input_btn_submit.connect_clicked(clone!(@strong tx_ui => move |_| {
        tx_ui.send(UiToThread::SerialInput(text_input.text().to_string())).unwrap();
        text_input.set_text("");
    }));

    text_input_button_box.append(&text_input_btn_submit);

    text_input_box.append(&text_input_button_box);

    column_serial.append(&text_input_frame);

    (column_serial, memory, buffer_serial)
}