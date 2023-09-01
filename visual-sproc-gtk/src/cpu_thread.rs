use gtk::glib::Sender;
use std::sync::mpsc::{Receiver, TryRecvError, RecvError};
use crate::messages::{UiToThread, ThreadToUi};


struct ThreadState {
    running: bool,
    multiplier: f64,
    run_thread: bool,
}

impl ThreadState {
    fn new() -> Self {
        Self {
            run_thread: true,
            running: false,
            multiplier: 1.0,
        }
    }

    fn handle_msg(&mut self, msg: UiToThread) -> Option<ThreadToUi> {
        match msg {
            UiToThread::CpuStart => self.running = true,
            UiToThread::CpuStop => self.running = false,
            UiToThread::Exit => self.run_thread = false,
            UiToThread::SetMultiplier(m) => { self.multiplier = m; println!("Setting to {m}"); },
            _ => (), // TODO - Process All Inputs
        }

        None
    }
}

pub fn cpu_thread(rx: Receiver<UiToThread>, tx: Sender<ThreadToUi>) {
    let mut state = ThreadState::new();

    'mainloop: while state.run_thread {
        if state.running {
            for _ in 0..1000 {
                let resp = match rx.try_recv() {
                    Ok(msg) => state.handle_msg(msg),
                    Err(TryRecvError::Disconnected) => break 'mainloop,
                    Err(TryRecvError::Empty) => break,
                };

                if let Some(r) = &resp {
                    tx.send(r.clone()).expect("Unable to send response to main thread!");
                }
            }
        } else {
            let resp = match rx.recv() {
                Ok(msg) => state.handle_msg(msg),
                Err(RecvError) => break 'mainloop,
            };

            if let Some(r) = &resp {
                tx.send(r.clone()).expect("Unable to send response to main thread!");
            }
        }

        // Step if required
        if state.running {
            // TODO: Step CPU
            std::thread::sleep(std::time::Duration::from_millis(100));
        }
    }

    let _ = tx.send(ThreadToUi::ThreadExit);
}
