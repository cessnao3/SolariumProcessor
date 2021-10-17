use fltk::prelude::*;
use fltk::{app::*, button::*, dialog::*, enums::*, window::*, text::*, group::*, frame::*, valuator::*};
use fltk::text::TextEditor;

use libscpu::cpu::SolariumCPU;
use libscpu::memory::{MemoryWord, ReadWriteSegment};

use libscpu_assemble::assemble;

use std::thread;
use std::time;

use std::sync::mpsc;

#[derive(Clone, Copy)]
enum Message
{
    Step,
    Start,
    Stop,
    Reset,
    Assemble,
    Tick,
    SetSpeed(f64)
}

#[derive(Clone)]
enum ThreadMessage
{
    SetMemory(Vec<MemoryWord>),
    Start,
    Stop,
    Reset,
    Exit,
    Step,
    SetSpeed(f64)
}

#[derive(Clone)]
enum GuiMessage
{
    UpdateRegisters([u16; SolariumCPU::NUM_REGISTERS]),
    LogMessage(String)
}

fn main()
{
    // Define the window values
    let app = App::default();

    // Initialize FLTK Sliders
    let (sender, receiver) = channel::<Message>();

    //let mut wind = Window::new(100, 100, 400, 300, "Hello from rust");
    let mut wind = Window::default()
        .with_size(1100, 600)
        .with_label("VisualSCPU");

    let mut main_group = Flex::default_fill().row();

    // Define the editor
    let mut editor_group = Flex::default_fill().column();
    let mut assembly_editor;
    {
        let mut assembly_label = Frame::default().with_label("Assembly Editor");
        assembly_editor = TextEditor::default();
        assembly_editor.set_buffer(TextBuffer::default());

        editor_group.set_size(&mut assembly_label, 50);

        let mut assemble_button = Button::default().with_label("Assemble");
        assemble_button.emit(sender, Message::Assemble);
        assembly_editor.set_linenumber_width(32);

        editor_group.set_size(&mut assemble_button, 50);
        editor_group.set_margin(10);
    }
    editor_group.end();

    // Define a logging text display
    let mut log_text_display;

    // Define Registers
    let mut register_group = Flex::default_fill().column();
    let register_labels;
    {
        // Add a simple step initiation button
        let mut button_group = Flex::default_fill().row();

        let mut step_button = Button::default().with_label("STEP");
        step_button.emit(sender, Message::Step);

        let mut start_button = Button::default().with_label("START");
        start_button.emit(sender, Message::Start);

        let mut stop_button = Button::default().with_label("STOP");
        stop_button.emit(sender, Message::Stop);

        let mut reset_button = Button::default().with_label("RESET");
        reset_button.emit(sender, Message::Reset);

        button_group.set_size(&mut step_button, 70);
        button_group.set_size(&mut start_button, 70);
        button_group.set_size(&mut stop_button, 70);
        button_group.set_size(&mut reset_button, 70);

        button_group.end();
        register_group.set_size(&mut button_group, 50);

        // Define teh speed slider
        let mut slider = HorValueSlider::default().with_label("Speed");
        slider.set_maximum(6.0);
        slider.set_minimum(2.0);
        slider.set_value(2.0);

        slider.set_callback(move |s| {
            sender.send(Message::SetSpeed((10.0f64).powf(s.value())));
        });

        register_group.set_size(&mut slider, 25);

        let mut slider_frame = Frame::default();
        register_group.set_size(&mut slider_frame, 25);

        // Define the registers and perform the initial update values
        let mut register_frame_label = Frame::default().with_label("Registers");
        register_group.set_size(&mut register_frame_label, 30);

        register_labels = define_registers(&mut register_group);

        // Add the log window
        log_text_display = TextDisplay::default();
        log_text_display.set_buffer(TextBuffer::default());
    }
    // End the register group
    register_group.end();
    register_group.set_margin(10);

    main_group.set_size(&mut register_group, 350);

    main_group.end();

    // Finish Window Setup
    wind.end();

    // Initialize thread communication channels
    let (gui_to_thread_tx, gui_to_thread_rx) = mpsc::channel();
    let (thread_to_gui_tx, thread_to_gui_rx) = mpsc::channel();

    // Setup the start/stop timer
    let cpu_thread = thread::spawn(move || {
        const THREAD_LOOP_MS: u64 = 10;
        const THREAD_LOOP_HZ: u64 = 1000 / 10;

        let mut step_repeat_count = 1;

        struct ProcessorStatusStruct
        {
            cpu: SolariumCPU,
            regs: [u16; SolariumCPU::NUM_REGISTERS],
            regs_updated: bool,
            step_error: bool,
            last_assembly: Vec::<MemoryWord>,
            msg_queue: Vec<GuiMessage>
        }

        impl ProcessorStatusStruct
        {
            fn new() -> ProcessorStatusStruct
            {
                let mut stat = ProcessorStatusStruct
                {
                    cpu: SolariumCPU::new(),
                    regs: [0u16; SolariumCPU::NUM_REGISTERS],
                    regs_updated: false,
                    step_error: false,
                    last_assembly: Vec::new(),
                    msg_queue: Vec::new()
                };

                stat.cpu.memory_map.add_segment(Box::new(ReadWriteSegment::new(
                    0,
                    (2usize).pow(16))));

                stat.reset();

                return stat;
            }

            fn reset(&mut self)
            {
                self.cpu.reset();
                self.update_regs();

                for (i, val) in self.last_assembly.iter().enumerate()
                {
                    self.cpu.memory_map.set(i as u16, *val);
                }
            }

            fn step(&mut self)
            {
                match self.cpu.step()
                {
                    Ok(()) => (),
                    Err(e) =>
                    {
                        self.msg_queue.push(GuiMessage::LogMessage(e));
                        self.step_error = true;
                    }
                }
                self.update_regs();
            }

            fn update_regs(&mut self)
            {
                for i in 0..SolariumCPU::NUM_REGISTERS
                {
                    self.regs[i] = self.cpu.get_register_value(i);
                }
                self.regs_updated = true;
            }

            fn load_data(&mut self, data: Vec::<MemoryWord>)
            {
                self.last_assembly = data;
                self.reset();

                self.msg_queue.push(GuiMessage::LogMessage(format!(
                    "loaded assembly code with length {0:}",
                    self.last_assembly.len())));
            }

            fn update_msg_queue(&mut self)
            {
                if self.regs_updated
                {
                    self.msg_queue.push(GuiMessage::UpdateRegisters(self.regs));
                    self.regs_updated = false;
                }
            }
        }

        let mut step_cpu = false;

        let mut cpu_stat = ProcessorStatusStruct::new();

        loop
        {
            let mut thread_error = false;

            for _ in 0..1000
            {
                match gui_to_thread_rx.try_recv()
                {
                    Ok(v) => match v
                    {
                        ThreadMessage::Exit =>
                        {
                            break;
                        },
                        ThreadMessage::Start =>
                        {
                            step_cpu = true;
                        },
                        ThreadMessage::Stop =>
                        {
                            step_cpu = false;
                        },
                        ThreadMessage::Reset =>
                        {
                            step_cpu = false;
                            cpu_stat.reset();
                        },
                        ThreadMessage::SetMemory(mem_vals) =>
                        {
                            step_cpu = false;
                            cpu_stat.load_data(mem_vals);

                        },
                        ThreadMessage::Step =>
                        {
                            cpu_stat.step();
                        },
                        ThreadMessage::SetSpeed(v) =>
                        {
                            step_repeat_count = (v / THREAD_LOOP_HZ as f64) as u64;
                        }
                    },
                    Err(mpsc::TryRecvError::Disconnected) => thread_error = true,
                    Err(mpsc::TryRecvError::Empty) => break
                };
            }

            if thread_error
            {
                break;
            }

            if step_cpu
            {
                let inner_repeat_count = step_repeat_count;

                for _ in 0..inner_repeat_count
                {
                    cpu_stat.step();
                }
            }

            cpu_stat.update_msg_queue();

            for msg in cpu_stat.msg_queue.iter()
            {
                match thread_to_gui_tx.send(msg.clone())
                {
                    Ok(()) => (),
                    Err(_) =>
                    {
                        break;
                    }
                }
            }

            if cpu_stat.msg_queue.len() > 0
            {
                cpu_stat.msg_queue.clear();
                sender.send(Message::Tick);
            }

            if cpu_stat.step_error
            {
                break;
            }

            thread::sleep(time::Duration::from_millis(THREAD_LOOP_MS));
        }
    });

    // Show the window and run the application
    wind.show();
    while app.wait()
    {
        let mut message_queue: Vec<String> = Vec::new();

        match thread_to_gui_rx.try_recv()
        {
            Ok(msg) =>
            {
                match msg
                {
                    GuiMessage::UpdateRegisters(regs) =>
                    {
                        for i in 0..SolariumCPU::NUM_REGISTERS
                        {
                            register_labels[i].buffer().unwrap().set_text(&format!("{0:}", regs[i]));
                        }
                    },
                    GuiMessage::LogMessage(err) =>
                    {
                        message_queue.push(err);
                    }
                }

                sender.send(Message::Tick);
            },
            Err(mpsc::TryRecvError::Empty) => (),
            Err(mpsc::TryRecvError::Disconnected) =>
            {
                alert_default("thread exit error!");
                break;
            }
        }

        let mut msg_to_send = None;

        if let Some(msg) = receiver.recv()
        {
            match msg
            {
                Message::Step =>
                {
                    msg_to_send = Some(ThreadMessage::Step);
                },
                Message::Start =>
                {
                    msg_to_send = Some(ThreadMessage::Start);
                },
                Message::Stop =>
                {
                    msg_to_send = Some(ThreadMessage::Stop);
                },
                Message::Reset =>
                {
                    msg_to_send = Some(ThreadMessage::Reset);
                },
                Message::Assemble =>
                {
                    match assembly_editor.buffer()
                    {
                        Some(v) =>
                        {
                            let lines = v.text().split('\n').map(|v| v.to_string()).collect();
                            let assembled_binary = assemble(lines);

                            match assembled_binary
                            {
                                Ok(v) =>
                                {
                                    msg_to_send = Some(ThreadMessage::SetMemory(v));
                                },
                                Err(e) =>
                                {
                                    message_queue.push(e);
                                }
                            };
                        },
                        None =>
                        {
                            alert_default("Unable to get assembly text buffer");
                            break;
                        }
                    };
                },
                Message::SetSpeed(v) =>
                {
                    msg_to_send = Some(ThreadMessage::SetSpeed(v));
                },
                Message::Tick => ()
            }
        }

        // Add all log message values
        {
            for msg in message_queue.iter()
            {
                // Add the text
                log_text_display.buffer().unwrap().append(&format!("{0:}\n", msg));

                // Scroll to end
                let num_lines = log_text_display.buffer().unwrap().text().split("\n").count();
                log_text_display.scroll(num_lines as i32, 0);
            }
        }

        match msg_to_send
        {
            Some(msg) =>
            {
                match gui_to_thread_tx.send(msg)
                {
                    Ok(()) => (),
                    Err(e) =>
                    {
                        alert_default(&format!("unable to send message to thread - {0:}", e));
                    }
                }
            },
            None => ()
        }
    }

    // Request closing the thread
    match gui_to_thread_tx.send(ThreadMessage::Exit)
    {
        Ok(()) =>
        {
            match cpu_thread.join()
            {
                Ok(()) => (),
                Err(_) => eprintln!("error joining to thread")
            }
        },
        Err(err) =>
        {
            eprintln!("thread already disconnected: {0:}", err.to_string());
        }
    }
}

fn define_registers(parent: &mut Flex) -> Vec<TextDisplay>
{
    let mut column_group = Flex::default_fill().column();

    let mut displays = Vec::new();

    assert!(SolariumCPU::NUM_REGISTERS % 2 == 0);

    for i in 0..SolariumCPU::NUM_REGISTERS / 2
    {
        let v = add_register_row(&mut column_group, i*2, i*2 + 1);
        for disp in v
        {
            displays.push(disp);
        }
    }

    column_group.end();

    parent.set_size(&mut column_group, 280);

    return displays;
}

fn add_register_row(parent: &mut Flex, reg0_ind: usize, reg1_ind: usize) -> Vec<TextDisplay>
{
    let mut displays = Vec::new();
    let mut row_group = Flex::default_fill().row();

    displays.push(setup_register(&mut row_group, reg0_ind));
    displays.push(setup_register(&mut row_group, reg1_ind));

    row_group.end();

    parent.set_size(&mut row_group, 30);

    return displays;
}

fn setup_register(parent: &mut Flex, index: usize) -> TextDisplay
{
    let mut label = Frame::default().with_label(&format!("R{0:}", index));
    let mut text_display = TextDisplay::default().with_align(Align::BottomRight);

    let buffer = TextBuffer::default();
    text_display.set_buffer(buffer);

    parent.set_size(&mut label, 50);
    parent.set_size(&mut text_display, 100);

    return text_display;
}
