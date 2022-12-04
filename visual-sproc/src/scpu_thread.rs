use super::messages::{GuiMessage, ThreadMessage};
use super::processor_state::ProcessorStatusStruct;

use std::thread;

use std::sync::mpsc;
use std::time;

pub fn run_scpu_thread(
    gui_to_thread_rx: mpsc::Receiver<ThreadMessage>,
    thread_to_gui_tx: mpsc::Sender<GuiMessage>,
) {
    const THREAD_LOOP_MS: u64 = 50;
    const THREAD_LOOP_HZ: u64 = 1000 / THREAD_LOOP_MS;

    let mut step_repeat_count = 1;

    let mut step_cpu = false;

    let mut cpu_stat = ProcessorStatusStruct::new();

    'main_loop: loop {
        let mut update_memory = false;

        for _ in 0..1000 {
            match gui_to_thread_rx.try_recv() {
                Ok(v) => match v {
                    ThreadMessage::Start => {
                        cpu_stat.clear_stop_request();
                        step_cpu = true;
                    }
                    ThreadMessage::Stop => {
                        step_cpu = false;
                        update_memory = true;
                    }
                    ThreadMessage::Reset => {
                        cpu_stat.clear_stop_request();
                        step_cpu = false;
                        cpu_stat.soft_reset();
                        update_memory = true;
                    }
                    ThreadMessage::SetMemory(mem_vals) => {
                        step_cpu = false;
                        cpu_stat.load_data(mem_vals);
                        update_memory = true;
                    }
                    ThreadMessage::HardwareInterrupt(hw_irq_num) => {
                        cpu_stat.hardware_interrupt(hw_irq_num);
                    }
                    ThreadMessage::Step => {
                        cpu_stat.clear_stop_request();
                        cpu_stat.step();
                        update_memory = true;
                    }
                    ThreadMessage::SetSpeed(v) => {
                        step_repeat_count = std::cmp::max((v / THREAD_LOOP_HZ as f64) as u64, 1);
                    }
                    ThreadMessage::SerialInput(char_buf) => {
                        for c in char_buf.into_iter() {
                            if c == '\0' {
                                break;
                            }
                            cpu_stat.push_serial_input_char(c);
                        }
                    }
                },
                Err(mpsc::TryRecvError::Disconnected) => {
                    break 'main_loop;
                }
                Err(mpsc::TryRecvError::Empty) => break,
            };
        }

        if cpu_stat.get_stop_request() {
            step_cpu = false;
            update_memory = true;
        }

        if step_cpu {
            let inner_repeat_count = step_repeat_count;

            for _ in 0..inner_repeat_count {
                if !cpu_stat.has_step_error() {
                    cpu_stat.step();
                } else {
                    break;
                }
            }
        }

        if update_memory {
            cpu_stat.send_memory_to_queue();
        }

        while let Some(c) = cpu_stat.pop_serial_output() {
            let output_char = match sproc::text::word_to_character(c) {
                Ok(v) => v,
                Err(e) => {
                    cpu_stat.add_log_to_queue(e.to_string());
                    '?'
                }
            };

            match thread_to_gui_tx.send(GuiMessage::SerialOutput(output_char)) {
                Ok(()) => (),
                Err(_) => {
                    break;
                }
            }
        }

        cpu_stat.update_msg_queue();

        for msg in cpu_stat.msg_queue.iter() {
            match thread_to_gui_tx.send(msg.clone()) {
                Ok(()) => (),
                Err(_) => {
                    break;
                }
            }
        }

        if cpu_stat.msg_queue.len() > 0 {
            cpu_stat.msg_queue.clear();
        }

        thread::sleep(time::Duration::from_millis(THREAD_LOOP_MS));
    }
}
