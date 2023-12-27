mod irq_clock;
mod serial_io;

pub use irq_clock::InterruptClockDevice;
pub use serial_io::SerialInputOutputDevice;

pub const DEVICE_MEM_SIZE: u32 = 32;

pub enum DeviceAction {
    CallInterrupt(u32),
}

pub trait ProcessorDevice {
    fn on_step(&mut self) -> Option<DeviceAction>;
}
