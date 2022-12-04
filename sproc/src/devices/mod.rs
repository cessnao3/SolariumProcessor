mod irq_clock;
mod serial_io;

pub use irq_clock::InterruptClockDevice;
pub use serial_io::SerialInputOutputDevice;

pub enum DeviceAction {
    CallInterrupt(usize),
}

pub trait SolariumDevice {
    fn on_step(&mut self) -> Option<DeviceAction>;
}
