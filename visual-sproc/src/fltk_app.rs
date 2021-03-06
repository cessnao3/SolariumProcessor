use fltk::enums::{Align, Color, Event, Font, FrameType, Key};
use fltk::image::PngImage;
use fltk::input::Input;
use fltk::prelude::*;
use fltk::table::{Table, TableRowSelectMode, TableContext};
use fltk::{app::*, draw, button::*, dialog::*, window::*, text::*, group::*, frame::*, valuator::*};

use super::messages::{ThreadMessage, GuiMessage, FltkMessage, SerialBuffer};

use super::fltk_registers::setup_register_group;

use sproc::common::MemoryWord;
use sproc::memory::MEM_MAX_SIZE;
use sda::assemble;

use spcc;

use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::{Arc, Mutex, mpsc};

fn get_app_icon() -> PngImage
{
    let logo_bytes = include_bytes!("../../doc/images/logo.png");
    return PngImage::from_data(logo_bytes).unwrap();
}

fn get_default_text() -> String
{
    let text_bytes = include_bytes!("../../examples/default.smc");
    return match std::str::from_utf8(text_bytes)
    {
        Ok(v) => v.to_string(),
        Err(e) => panic!("UTF-8 Error: {0:}", e.to_string())
    };
}

#[derive(Debug, Clone, Copy)]
struct TickTimerState
{
    pub tick_running: bool,
    pub thread_exit: bool
}

impl TickTimerState
{
    pub fn new() -> Self
    {
        return Self
        {
            tick_running: false,
            thread_exit: false
        };
    }
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
        .with_label("V/SProc");

    main_window.handle(move |_, event|
    {
        return event == Event::KeyDown && event_key() == Key::Escape;
    });

    main_window.set_icon(Some(get_app_icon()));
    main_window.make_resizable(true);

    let mut main_group = Flex::default_fill().row();

    // Define the editor
    let mut code_editor;
    {
        let mut editor_group = Flex::default_fill().column();

        let mut editor_label = Frame::default().with_label("Code Editor");
        editor_group.set_size(&mut editor_label, 20);

        code_editor = TextEditor::default();
        code_editor.set_buffer(TextBuffer::default());
        code_editor.set_linenumber_width(48);

        let mut button_group = Flex::default_fill().row();

        let mut assemble_button = Button::default().with_label("Assemble");
        assemble_button.emit(fltk_sender, FltkMessage::Assemble);

        let mut compile_button = Button::default().with_label("Compile");
        compile_button.emit(fltk_sender, FltkMessage::Compile);

        let mut compile_text_button = Button::default().with_label("C->Text");
        compile_text_button.emit(fltk_sender, FltkMessage::CompileToText);

        button_group.set_margin(5);
        button_group.end();

        editor_group.set_size(&mut button_group, 50);
        editor_group.set_margin(10);

        editor_group.end();
    }

    // Define the memory Values
    let shared_table_memory = Rc::new(RefCell::new(Vec::<MemoryWord>::new()));
    shared_table_memory.borrow_mut().resize(MEM_MAX_SIZE, MemoryWord::new(0));

    // Define the memory group values
    let mut serial_output;
    let mut serial_input;
    let mut memory_table;
    {
        let mut memory_group = Flex::default_fill().column();

        let mut memory_label = Frame::default().with_label("Memory");
        memory_group.set_size(&mut memory_label, 20);

        let inner_memory = shared_table_memory.clone();

        let mem_size = inner_memory.borrow().len() as i32;
        let num_cols = 4;
        assert!(num_cols > 0);
        assert_eq!(mem_size % num_cols, 0);

        let num_rows = mem_size / num_cols;

        memory_table = Table::default_fill();
        memory_table.set_row_header(true);
        memory_table.set_col_header(true);
        memory_table.set_type(TableRowSelectMode::None);
        memory_table.set_rows(num_rows);
        memory_table.set_cols(num_cols);
        memory_table.set_col_width_all(64);
        memory_table.set_row_height_all(18);
        memory_table.draw_cell(
            move |_, ctx, row, col, x, y, width, height|
            {
                match ctx
                {
                    TableContext::StartPage => draw::set_font(Font::Helvetica, 14),
                    TableContext::ColHeader =>
                    {
                        draw::push_clip(x, y, width, height);
                        draw::draw_box(FrameType::ThinUpBox, x, y, width, height, Color::FrameDefault);
                        draw::set_draw_color(Color::Black);
                        draw::draw_text2(&format!("{0:}", col), x, y, width, height, Align::Center);
                        draw::pop_clip();
                    },
                    TableContext::RowHeader =>
                    {
                        draw::push_clip(x, y, width, height);
                        draw::draw_box(FrameType::ThinUpBox, x, y, width, height, Color::FrameDefault);
                        draw::set_draw_color(Color::Black);
                        draw::draw_text2(&format!("{0:04X}", row * num_cols), x, y, width, height, Align::Center);
                        draw::pop_clip();
                    },
                    TableContext::Cell =>
                    {
                        let index_val = (row * num_cols + col) as usize;
                        let val = inner_memory.borrow()[index_val];
                        draw::push_clip(x, y, width, height);
                        draw::draw_rect_fill(x, y, width, height, Color::White);
                        draw::draw_box(FrameType::ThinDownFrame, x, y, width, height, Color::Black);
                        draw::set_draw_color(Color::Black);
                        draw::draw_text2(&format!("{0:04X}", val.get()), x, y, width, height, Align::Center);
                        draw::pop_clip();
                    },
                    _ => ()
                };
            }
        );
        memory_table.end();

        let mut serial_label = Frame::default().with_label("Serial Output");
        memory_group.set_size(&mut serial_label, 20);

        serial_output = TextDisplay::default();
        serial_output.set_buffer(TextBuffer::default());

        let mut serial_input_label = Frame::default().with_label("Serial Input");
        memory_group.set_size(&mut serial_input_label, 20);

        serial_input = Input::default();
        memory_group.set_size(&mut serial_input, 28);
        serial_input.handle(move |input, event|
        {
            if event == Event::KeyDown
            {
                if event_key() == Key::Enter
                {
                    let mut input_string = input.value();
                    input_string.push('\n');

                    let box_char = Box::new(input_string.chars().collect::<Vec<_>>());
                    fltk_sender.send(FltkMessage::SerialInput(SerialBuffer::from_box(box_char)));

                    input.set_value("");

                    return true;
                }
            }
            return false;
        });

        // Set the load input from file
        let mut serial_file_load_button = Button::default().with_label("Load File");
        memory_group.set_size(&mut serial_file_load_button, 20);

        serial_file_load_button.set_callback(move |_|
        {
            let mut file_browser = NativeFileChooser::new(FileDialogType::BrowseFile);
            file_browser.show();

            let path: PathBuf = file_browser.filename();

            let str_vals = match std::fs::read_to_string(path)
            {
                Ok(v) => v,
                Err(_) => {
                    fltk_sender.send(FltkMessage::FileLoadError);
                    return;
                }
            };

            let box_char = Box::new(str_vals.replace('\r', "").chars().collect::<Vec<_>>());
            fltk_sender.send(FltkMessage::SerialInput(SerialBuffer::from_box(box_char)));
        });

        memory_group.set_margin(10);
        memory_group.end();
    }

    // Set initial text
    code_editor.buffer().unwrap().set_text(&get_default_text());

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

        let mut hardware_irq_button = Button::default().with_label("IRQ 0");
        hardware_irq_button.emit(fltk_sender, FltkMessage::HardwareInterrupt(0));

        button_group.end();
        register_group.set_size(&mut button_group, 40);

        // Define the speed slider
        let mut speed_slider = HorValueSlider::default().with_label("Speed");
        speed_slider.set_maximum(6.0);
        speed_slider.set_minimum(1.0);
        speed_slider.set_value(1.0);
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

    // Show the window and run the application
    main_window.show();
    let mut message_queue: Vec<String> = Vec::new();
    let mut serial_output_queue: Vec<char> = Vec::new();

    // Create a thread timer callback
    let tick_thread_state = Arc::<Mutex::<TickTimerState>>::new(Mutex::<TickTimerState>::new(TickTimerState::new()));
    let tick_thread_join;
    {
        let thread_arc = tick_thread_state.clone();

        tick_thread_join = std::thread::spawn(move || {
            loop
            {
                match thread_arc.lock()
                {
                    Ok(mut v) =>
                    {
                        if v.thread_exit
                        {
                            break;
                        }

                        if !v.tick_running
                        {
                            fltk_sender.send(FltkMessage::Tick);
                            v.tick_running = true;
                        }
                    },
                    Err(_) => panic!("Tick Thread Exited!")
                };
                std::thread::sleep(std::time::Duration::from_secs_f64(1.0 / 30.0));
            }
        });
    }

    while app.wait()
    {
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
                                register_labels[i].buffer().unwrap().set_text(&format!("{0:04X}", regs[i]));
                            }
                        },
                        GuiMessage::LogMessage(err) =>
                        {
                            message_queue.push(err);
                        },
                        GuiMessage::UpdateMemory(new_memory_vals) =>
                        {
                            for (ind, val) in new_memory_vals.iter().enumerate()
                            {
                                shared_table_memory.borrow_mut()[ind].set(val.get());
                            }
                            memory_table.redraw();
                        },
                        GuiMessage::SerialOutput(c) =>
                        {
                            serial_output_queue.push(c);
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
                FltkMessage::HardwareInterrupt(int_val) =>
                {
                    msg_to_send = Some(ThreadMessage::HardwareInterrupt(int_val));
                },
                FltkMessage::SerialInput(c) =>
                {
                    let data = unsafe { Box::from_raw(c.ptr) };
                    msg_to_send = Some(ThreadMessage::SerialInput(data));
                },
                FltkMessage::FileLoadError =>
                {
                    message_queue.push("Unable to load file".to_string());
                },
                FltkMessage::Assemble | FltkMessage::Compile | FltkMessage::CompileToText =>
                {
                    match code_editor.buffer()
                    {
                        Some(mut v) =>
                        {
                            let editor_text = v.text();

                            let lines_opt = match msg
                            {
                                FltkMessage::Assemble => Some(editor_text.split('\n').map(|v| v.to_string()).collect()),
                                FltkMessage::Compile | FltkMessage::CompileToText => {
                                    match spcc::compile(&editor_text)
                                    {
                                        Ok(s) => Some(s),
                                        Err(e) => {
                                            message_queue.push(e);
                                            None
                                        }
                                    }
                                },
                                _ => panic!("unexpected message provided")
                            };

                            if let Some(lines) = lines_opt
                            {
                                match msg
                                {
                                    FltkMessage::CompileToText =>
                                    {
                                        v.set_text(&lines.join("\n"));
                                    }
                                    _ => ()
                                }

                                let assembled_binary = assemble(&lines.iter().map(|v| v.as_str()).collect::<Vec<_>>());

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
                            }
                        },
                        None =>
                        {
                            alert_default("Unable to get assembly text buffer");
                        }
                    };
                    serial_output.buffer().unwrap().set_text("");
                },
                FltkMessage::SetSpeed(new_speed) =>
                {
                    msg_to_send = Some(ThreadMessage::SetSpeed(new_speed));
                },
                FltkMessage::Tick =>
                {
                    if let Ok(mut v) = tick_thread_state.lock()
                    {
                        v.tick_running = false;
                    }
                }
            }
        }

        // Add all log message values
        for msg in message_queue.iter()
        {
            // Add the text
            log_text_display.buffer().unwrap().append(&format!("{0:}\n", msg));

            // Scroll to end
            let num_lines = log_text_display.buffer().unwrap().text().split("\n").count();
            log_text_display.scroll(num_lines as i32, 0);
        }
        message_queue.clear();

        // Add the serial output values
        if !serial_output_queue.is_empty()
        {
            serial_output.buffer().unwrap().append(&serial_output_queue.iter().collect::<String>());
            serial_output_queue.clear();
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

    // Exit the timer thread
    if let Ok(mut v) = tick_thread_state.lock()
    {
        v.thread_exit = true;
    }
    else
    {
        panic!("Unable to acquire mutex");
    }

    tick_thread_join.join().expect("couldn't join thread result");
}
