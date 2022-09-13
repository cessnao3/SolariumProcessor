mod serial_io;

pub use serial_io::SerialInputOutputDevice;

use crate::memory::MemorySegment;

pub enum DeviceAction
{
    CallInterrupt(usize)
}

pub trait SolariumDevice : MemorySegment
{
    fn on_step(&mut self) -> Option<DeviceAction>;
}
