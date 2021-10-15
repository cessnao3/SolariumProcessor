use fltk::prelude::*;
use fltk::{app::*, button::*, dialog::*, enums::*, window::*, text::*, group::*, frame::*,};
use fltk::text::TextEditor;

use libscpu::cpu::SolariumCPU;
use libscpu::memory::ReadWriteSegment;

use std::thread;
use std::time;

use std::sync::mpsc;

#[derive(Clone, Copy)]
enum Message
{
    Step,
    UpdateRegisters,
    Start,
    Stop,
    Reset,
    Tick,
    Assemble,
    Log
}

#[derive(Clone, Copy)]
enum ThreadMessage
{
    Start,
    Stop,
    Exit
}

fn main()
{
    // Define the processor
    let mut cpu = SolariumCPU::new();
    cpu.memory_map.add_segment(Box::new(ReadWriteSegment::new(0, 10000)));

    // Define the window values
    let app = App::default();

    let (sender, receiver) = channel::<Message>();

    //let mut wind = Window::new(100, 100, 400, 300, "Hello from rust");
    let mut wind = Window::default()
        .with_size(1100, 600)
        .with_label("VisualSCPU");

    let mut main_group = Flex::default_fill().row();
    let mut assembly_editor;

    let mut editor_group = Flex::default_fill().column();
    {
        let mut assembly_label = Frame::default().with_label("Assembly Editor");
        assembly_editor = TextEditor::default();
        assembly_editor.set_buffer(TextBuffer::default());

        editor_group.set_size(&mut assembly_label, 50);

        let mut assemble_button = Button::default().with_label("Assemble");
        assemble_button.emit(sender, Message::Assemble);

        editor_group.set_size(&mut assemble_button, 50);
    }
    editor_group.end();

    //column_group.set_size(&mut text_editor, 100);

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

    main_group.set_size(&mut register_group, 350);

    main_group.end();

    // Finish Window Setup
    wind.end();

    // Init Parameters
    sender.send(Message::UpdateRegisters);

    let (thread_tx, thread_rx) = mpsc::channel();

    //thread_tx.send(ThreadMessage::Start);

    // Setup the start/stop timer
    thread::spawn(move || {
        let mut step_cpu = false;
        loop {
            match thread_rx.try_recv()
            {
                Ok(v) => match v
                {
                    ThreadMessage::Exit => break,
                    ThreadMessage::Start => step_cpu = true,
                    ThreadMessage::Stop => step_cpu = false
                },
                Err(mpsc::TryRecvError::Disconnected) => break,
                Err(mpsc::TryRecvError::Empty) => ()
            };

            if step_cpu
            {
                thread::sleep(time::Duration::from_millis(10));
                sender.send(Message::Tick);
            }
    }});

    // Show the window and run the application
    wind.show();
    while app.wait()
    {
        if let Some(msg) = receiver.recv() {
            match msg {
                Message::Step | Message::Tick =>
                {
                    match cpu.step()
                    {
                        Ok(_) =>
                        {
                            println!("Step Success");
                        }
                        Err(e) =>
                        {
                            alert_default(&format!("Step Error: {0:}", e));
                            println!("Step Error: {0:}", e)
                        }
                    }

                    sender.send(Message::UpdateRegisters);
                },
                Message::UpdateRegisters =>
                {
                    for i in 0..SolariumCPU::NUM_REGISTERS
                    {
                        register_labels[i].buffer().unwrap().set_text(&format!("{0:}", cpu.get_register_value(i)));
                    }
                },
                Message::Start =>
                {
                    thread_tx.send(ThreadMessage::Start);
                },
                Message::Stop =>
                {
                    thread_tx.send(ThreadMessage::Stop);
                },
                Message::Reset =>
                {
                    thread_tx.send(ThreadMessage::Stop);
                    cpu.reset();
                },
                Message::Assemble => (),
                Message::Log => ()
            }
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
