use fltk::enums::Event;
use fltk::image::PngImage;
use fltk::prelude::*;
use fltk::{app::*, button::*, dialog::*, window::*, text::*, group::*, frame::*, valuator::*};

use super::messages::{ThreadMessage, GuiMessage, FltkMessage};

use super::fltk_registers::setup_register_group;

use libsproc::common::MemoryWord;
use libsproc_assemble::assemble;

use std::sync::{Arc, Mutex, mpsc};

fn get_app_icon() -> PngImage
{
    let logo_bytes = include_bytes!("../../doc/images/logo.png");
    return PngImage::from_data(logo_bytes).unwrap();
}

fn get_default_text() -> String
{
    let text_bytes = include_bytes!("../../examples/counter.smc");
    return match std::str::from_utf8(text_bytes)
    {
        Ok(v) => v.to_string(),
        Err(e) => panic!("UTF-8 Error: {0:}", e.to_string())
    };
}

pub fn setup_and_run_app(
    gui_to_thread_tx: mpsc::Sender<ThreadMessage>,
    thread_to_gui_rx: mpsc::Receiver<GuiMessage>)
{
    // Define the window values
    let app = App::default();

    // Initialize FLTK Senders
    let (fltk_sender, fltk_receiver) = channel::<FltkMessage>();

    let mut main_window = Window::default()
        .with_size(1100, 600)
        .with_label("VisualSProc");

    main_window.set_callback(move |_|
    {
        // Handle the close app event
        if fltk::app::event() == Event::Close
        {
            app.quit();
        }
    });

    main_window.set_icon(Some(get_app_icon()));

    let mut main_group = Flex::default_fill().row();

    // Define the editor
    let mut editor_group = Flex::default_fill().column();
    let mut assembly_editor;
    {
        let mut assembly_label = Frame::default().with_label("Assembly Editor");
        assembly_editor = TextEditor::default();
        assembly_editor.set_buffer(TextBuffer::default());

        editor_group.set_size(&mut assembly_label, 20);

        let mut assemble_button = Button::default().with_label("Assemble");
        assemble_button.emit(fltk_sender, FltkMessage::Assemble);
        assembly_editor.set_linenumber_width(48);

        editor_group.set_size(&mut assemble_button, 50);
        editor_group.set_margin(10);
    }
    editor_group.end();

    // Set initial text
    assembly_editor.buffer().unwrap().set_text(&get_default_text());

    // Define a logging text display
    let mut log_text_display;

    // Define Registers
    let mut register_group = Flex::default_fill().column();
    let register_labels;
    {
        // Add a simple step initiation button
        let mut button_group = Flex::default_fill().row();

        let mut step_button = Button::default().with_label("STEP");
        step_button.emit(fltk_sender, FltkMessage::Step);

        let mut start_button = Button::default().with_label("START");
        start_button.emit(fltk_sender, FltkMessage::Start);

        let mut stop_button = Button::default().with_label("STOP");
        stop_button.emit(fltk_sender, FltkMessage::Stop);

        let mut reset_button = Button::default().with_label("RESET");
        reset_button.emit(fltk_sender, FltkMessage::Reset);

        button_group.set_size(&mut step_button, 70);
        button_group.set_size(&mut start_button, 70);
        button_group.set_size(&mut stop_button, 70);
        button_group.set_size(&mut reset_button, 70);

        button_group.end();
        register_group.set_size(&mut button_group, 50);

        // Define the speed slider
        let mut speed_slider = HorValueSlider::default().with_label("Speed");
        speed_slider.set_maximum(6.0);
        speed_slider.set_minimum(2.0);
        speed_slider.set_value(2.0);
        speed_slider.set_precision(1);
        speed_slider.set_callback(move |v|
        {
            fltk_sender.send(FltkMessage::SetSpeed((10.0f64).powf(v.value())));
        });

        register_group.set_size(&mut speed_slider, 25);

        let mut slider_frame = Frame::default();
        register_group.set_size(&mut slider_frame, 25);

        // Define the registers and perform the initial update values
        let mut register_frame_label = Frame::default().with_label("Registers");
        register_group.set_size(&mut register_frame_label, 30);

        register_labels = setup_register_group(&mut register_group);

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
    main_window.end();

    // Define whether the callback has been triggered
    let callback_finished = Arc::new(Mutex::new(true));

    // Show the window and run the application
    main_window.show();
    while app.wait()
    {
        let mut message_queue: Vec<String> = Vec::new();

        let mut thread_exit = false;

        for _ in 0..10000
        {
            match thread_to_gui_rx.try_recv()
            {
                Ok(msg) =>
                {
                    match msg
                    {
                        GuiMessage::UpdateRegisters(regs) =>
                        {
                            for i in 0..regs.len()
                            {
                                register_labels[i].buffer().unwrap().set_text(&format!("{0:}", regs[i]));
                            }
                        },
                        GuiMessage::LogMessage(err) =>
                        {
                            message_queue.push(err);
                        }
                    }
                },
                Err(mpsc::TryRecvError::Empty) => break,
                Err(mpsc::TryRecvError::Disconnected) =>
                {
                    alert_default("thread exit error!");
                    thread_exit = true;
                    break;
                }
            }
        }

        if thread_exit
        {
            break;
        }

        let mut msg_to_send = None;

        if let Some(msg) = fltk_receiver.recv()
        {
            match msg
            {
                FltkMessage::Step =>
                {
                    msg_to_send = Some(ThreadMessage::Step);
                },
                FltkMessage::Start =>
                {
                    msg_to_send = Some(ThreadMessage::Start);
                },
                FltkMessage::Stop =>
                {
                    msg_to_send = Some(ThreadMessage::Stop);
                },
                FltkMessage::Reset =>
                {
                    msg_to_send = Some(ThreadMessage::Reset);
                },
                FltkMessage::Assemble =>
                {
                    match assembly_editor.buffer()
                    {
                        Some(v) =>
                        {
                            let assembly_text = v.text();
                            let lines = assembly_text.split('\n').map(|v| v).collect();
                            let assembled_binary = assemble(lines);

                            match assembled_binary
                            {
                                Ok(v) =>
                                {
                                    msg_to_send = Some(ThreadMessage::SetMemory(v.iter().map(|v| MemoryWord::new(*v)).collect()));
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
                        }
                    };
                },
                FltkMessage::SetSpeed(new_speed) =>
                {
                    msg_to_send = Some(ThreadMessage::SetSpeed(new_speed));
                },
                FltkMessage::Tick => ()
            }
        }

        // Setup a new callback value to ensure that the event counter is updated at 30 Hzs
        let mut callback_val = callback_finished.lock().unwrap();
        if *callback_val && main_window.visible()
        {
            *callback_val = false;
            let callback_finished_cb = Arc::clone(&callback_finished);
            add_timeout(1.0 / 30.0, move ||
            {
                fltk_sender.send(FltkMessage::Tick);
                let mut data = callback_finished_cb.lock().unwrap();
                *data = true;
            });
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

        // Send messages
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
}
