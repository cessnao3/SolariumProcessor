use fltk::prelude::*;
use fltk::{app::*, button::*, dialog::*, enums::*, window::*, text::*, group::*, frame::*,};
use fltk::text::TextEditor;

use libscpu::cpu::SolariumCPU;
use libscpu::memory::{ReadWriteSegment, MemoryWord};

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
    Tick
}

#[derive(Clone)]
enum ThreadMessage
{
    SetMemory(Vec<MemoryWord>),
    Start,
    Stop,
    Reset,
    Exit,
    Step
}

#[derive(Clone)]
enum GuiMessage
{
    UpdateRegisters([u16; SolariumCPU::NUM_REGISTERS]),
    Error(String)
}

fn main()
{
    // Define the window values
    let app = App::default();

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

        editor_group.set_size(&mut assemble_button, 50);
        editor_group.set_margin(10);
    }
    editor_group.end();

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

        // Define the registers and perform the initial update values
        register_labels = define_registers(&mut register_group);
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
        let mut step_cpu = false;

        let mut cpu = SolariumCPU::new();
        cpu.memory_map.add_segment(Box::new(ReadWriteSegment::new(0, 10000)));

        let mut regs = [0u16; SolariumCPU::NUM_REGISTERS];

        let mut reset = true;

        let mut last_assembly = Vec::<MemoryWord>::new();

        loop
        {
            let mut single_step = false;
            let mut msg_to_send = None;

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
                        cpu.reset();
                        step_cpu = false;
                        reset = true;
                    },
                    ThreadMessage::SetMemory(mem_vals) =>
                    {
                        cpu.reset();
                        step_cpu = false;
                        reset = true;
                        last_assembly = mem_vals;
                    },
                    ThreadMessage::Step =>
                    {
                        single_step = true;
                    }
                },
                Err(mpsc::TryRecvError::Disconnected) => break,
                Err(mpsc::TryRecvError::Empty) => ()
            };

            if step_cpu || single_step
            {
                match cpu.step()
                {
                    Ok(()) => (),
                    Err(err) => msg_to_send = Some(GuiMessage::Error(err))
                };
            }

            if (step_cpu || single_step || reset) && msg_to_send.is_none()
            {
                for i in 0..SolariumCPU::NUM_REGISTERS
                {
                    regs[i] = cpu.get_register_value(i);
                }

                msg_to_send = Some(GuiMessage::UpdateRegisters(regs));
            }

            if reset
            {
                reset = false;

                for (i, val) in last_assembly.iter().enumerate()
                {
                    cpu.memory_map.set(i as u16, *val);
                }
            }

            if msg_to_send.is_some()
            {
                match thread_to_gui_tx.send(msg_to_send.unwrap())
                {
                    Ok(()) =>
                    {
                        sender.send(Message::Tick);
                    },
                    Err(_) =>
                    {
                        break;
                    }
                }
            }

            thread::sleep(time::Duration::from_millis(10));
        }
    });

    // Show the window and run the application
    wind.show();
    while app.wait()
    {
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
                    GuiMessage::Error(err) =>
                    {
                        alert_default(&err);
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
                                Err(e) => alert_default(&e)
                            };
                        },
                        None => alert_default("Unable to get assembly text buffer")
                    };
                },
                Message::Tick => ()
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
                Err(_) => eprintln!("error closing thread")
            }
        },
        Err(_) =>
        {
            eprintln!("thread already marked as closed!");
        }
    }


}

fn define_registers(parent: &mut Flex) -> Vec<TextDisplay>
{
    let mut column_group = Flex::default_fill().column();

    let mut displays = Vec::new();

    for i in 0..8usize
    {
        let v = add_register_row(&mut column_group, i*2, i*2 + 1);
        for disp in v
        {
            displays.push(disp);
        }
    }

    column_group.end();

    parent.set_size(&mut column_group, 100);

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
