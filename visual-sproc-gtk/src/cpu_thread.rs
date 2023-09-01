use gtk::glib::Sender;
use std::sync::mpsc::{Receiver, TryRecvError};
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
            UiToThread::SetMultiplier(m) => self.multiplier = m,
            _ => (), // TODO - Process All Inputs
        }

        None
    }
}

pub fn cpu_thread(rx: Receiver<UiToThread>, tx: Sender<ThreadToUi>) {
    let mut state = ThreadState::new();

    'mainloop: while state.run_thread {
        // TODO - Change from try_recv to recv if not running so we don't need sleep

        // Process Inputs
        let max_val = if state.running { 1000 } else { 1 };
        for _ in 0..max_val {
            let resp: Option<ThreadToUi> = match rx.try_recv() {
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

    let _ = tx.send(ThreadToUi::ThreadExit);
}
