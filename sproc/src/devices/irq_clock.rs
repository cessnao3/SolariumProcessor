use super::{DeviceAction, SolariumDevice};

pub struct InterruptClockDevice {
    clock_interval: usize,
    current_count: usize,
    interrupt: usize,
}

impl InterruptClockDevice {
    pub fn new(interval: usize, interrupt: usize) -> Self {
        if interval == 0 {
            panic!("interval must be a positive integer")
        }

        return Self {
            clock_interval: interval,
            current_count: 0,
            interrupt,
        };
    }
}

impl SolariumDevice for InterruptClockDevice {
    fn on_step(&mut self) -> Option<DeviceAction> {
        self.current_count = (self.current_count + 1) % self.clock_interval;

        return match self.current_count {
            0 => Some(DeviceAction::CallInterrupt(self.interrupt)),
            _ => None,
        };
    }
}
