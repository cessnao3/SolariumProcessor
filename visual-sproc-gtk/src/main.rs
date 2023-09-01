//use gtk::glib::clone;
use gtk::{glib, Application, ApplicationWindow};
use gtk::{prelude::*, Stack, StackSwitcher};
use gtk::{Box, Button, Frame, ScrolledWindow, TextBuffer, TextView};

use std::default;
use std::sync::mpsc::{channel, Receiver, Sender, TryRecvError, SendError};

const APP_ID: &str = "com.orourke.Solarium.VSProc";

fn main() -> glib::ExitCode {
    // Create a new application
    let app = Application::builder().application_id(APP_ID).build();

    // Connect to "activate" signal of `app`
    app.connect_activate(build_ui);

    // Run the application
    app.run()
}

enum UiToThread {
    CpuStep,
    CpuStart,
    CpuStop,
    CpuReset,
    CpuIrq(u16),
    SetCode(Vec<u16>),
    SerialInput(String),
    RequestMemory(u16, u16),
    SetMultiplier(i32),
    Exit,
}

enum ThreadToUi {
    ResponseMemory(u16, Vec<u16>),
    SerialOutput(String),
    LogMessage(String),
    RegisterState([u16; 16]),
}

fn build_ui(app: &Application) {
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

struct ThreadState {
    running: bool,
    multiplier: i32,
    run_thread: bool,
}

impl ThreadState {
    fn new() -> Self {
        Self {
            run_thread: true,
            running: false,
            multiplier: 1,
        }
    }

    fn handle_msg(&mut self, msg: UiToThread) -> Option<ThreadToUi> {
        match msg {
            UiToThread::CpuStart => self.running = true,
            UiToThread::CpuStop => self.running = false,
            UiToThread::Exit => self.run_thread = false,
            UiToThread::SetMultiplier(m) => self.multiplier = m,
            _ => (), // TODO - Process All Inputs
        }

        None
    }
}

fn cpu_thread(rx: Receiver<UiToThread>, tx: Sender<ThreadToUi>) {
    let mut state = ThreadState::new();

    'mainloop: while state.run_thread {
        // Process Inputs
        let max_val = if state.running { 1000 } else { 1 };
        for _ in 0..max_val {
            let resp = match rx.try_recv() {
                Ok(msg) => state.handle_msg(msg),
                Err(TryRecvError::Disconnected) => break 'mainloop,
                Err(TryRecvError::Empty) => break,
            };

            if let Some(r) = resp {
                tx.send(r).expect("Unable to send response to main thread!");
            }
        }

        // Step if required
        if state.running {
            // TODO: Step CPU
        }

        std::thread::sleep(std::time::Duration::from_millis(100));
    }
}
