mod processor_state;
mod messages;
mod scpu_thread;

mod fltk_app;
mod fltk_registers;

use std::thread;
use std::sync::mpsc;

use scpu_thread::run_scpu_thread;
use fltk_app::setup_and_run_app;

fn main()
{
    // Define the CPU Thread join handle
    let cpu_thread;

    {
        // Initialize thread communication channels
        let (gui_to_thread_tx, gui_to_thread_rx) = mpsc::channel();
        let (thread_to_gui_tx, thread_to_gui_rx) = mpsc::channel();

        // Setup the start/stop timer
        cpu_thread = thread::spawn(move || {
            run_scpu_thread(gui_to_thread_rx, thread_to_gui_tx);
        });

        // Run the main application
        setup_and_run_app(
            gui_to_thread_tx,
            thread_to_gui_rx);
    }

    // Wait for thread to exit
    match cpu_thread.join()
    {
        Ok(()) => (),
        Err(_) => eprintln!("error joining to thread")
    }
}
