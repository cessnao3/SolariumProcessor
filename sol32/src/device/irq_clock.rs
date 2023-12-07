use super::{DeviceAction, ProcessorDevice};

pub struct InterruptClockDevice {
    clock_interval: u32,
    current_count: u32,
    interrupt: u32,
}

impl InterruptClockDevice {
    pub fn new(interval: u32, interrupt: u32) -> Self {
        if interval == 0 {
            panic!("interval must be a positive integer")
        }

        Self {
            clock_interval: interval,
            current_count: 0,
            interrupt,
        }
    }
}

impl ProcessorDevice for InterruptClockDevice {
    fn on_step(&mut self) -> Option<DeviceAction> {
        self.current_count = (self.current_count + 1) % self.clock_interval;

        match self.current_count {
            0 => Some(DeviceAction::CallInterrupt(self.interrupt)),
            _ => None,
        }
    }
}
