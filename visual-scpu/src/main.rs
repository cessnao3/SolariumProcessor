mod processor_state;
mod messages;

mod fltk_registers;

use fltk::enums::Event;
use fltk::prelude::*;
use fltk::{app::*, button::*, dialog::*, window::*, text::*, group::*, frame::*, valuator::*};

use processor_state::ProcessorStatusStruct;
use messages::*;

use fltk_registers::setup_register_group;

use libscpu_assemble::assemble;

use std::thread;
use std::time;

use std::sync::{Arc, Mutex, mpsc};

fn main()
{
    // Define the window values
    let app = App::default();

    // Initialize FLTK Senders
    let (fltk_sender, fltk_receiver) = channel::<FltkMessage>();

    let mut main_window = Window::default()
        .with_size(1100, 600)
        .with_label("VisualSCPU");

    main_window.set_callback(move |_|
    {
        // Handle the close app event
        if fltk::app::event() == Event::Close
        {
            app.quit();
        }
    });

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
        assemble_button.emit(fltk_sender, FltkMessage::Assemble);
        assembly_editor.set_linenumber_width(48);

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
        speed_slider.set_maximum(5.0);
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

    // Initialize thread communication channels
    let (gui_to_thread_tx, gui_to_thread_rx) = mpsc::channel();
    let (thread_to_gui_tx, thread_to_gui_rx) = mpsc::channel();

    // Setup the start/stop timer
    let cpu_thread = thread::spawn(move || {
        const THREAD_LOOP_MS: u64 = 10;
        const THREAD_LOOP_HZ: u64 = 1000 / 10;

        let mut step_repeat_count = 1;

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
                            thread_error = true;
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
                    Err(mpsc::TryRecvError::Disconnected) =>
                    {
                        thread_error = true;
                        break;
                    },
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
            }

            if cpu_stat.has_step_error()
            {
                break;
            }

            thread::sleep(time::Duration::from_millis(THREAD_LOOP_MS));
        }
    });

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

        // Setup a new callback value
        let mut callback_val = callback_finished.lock().unwrap();
        if *callback_val
        {
            *callback_val = false;
            let callback_finished_cb = Arc::clone(&callback_finished);
            add_timeout(0.01, move ||
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

    // Send the thread exit message
    match gui_to_thread_tx.send(ThreadMessage::Exit)
    {
        Ok(()) => (),
        Err(_) => eprintln!("unable to send exit message to thread")
    };

    // Wait for thread to exit
    match cpu_thread.join()
    {
        Ok(()) => (),
        Err(_) => eprintln!("error joining to thread")
    }
}
