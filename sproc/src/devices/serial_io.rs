use std::cell::RefCell;
use std::collections::VecDeque;

use super::{DeviceAction, SolariumDevice};

use crate::common::{MemoryWord, SolariumDeviceError, SolariumError};
use crate::memory::MemorySegment;

/// Provides a memory serial I/O memory-mapped device
pub struct SerialInputOutputDevice {
    /// Provides the base address for the input device
    input_queue: RefCell<VecDeque<MemoryWord>>,
    output_queue: VecDeque<MemoryWord>,
    buffer_size: usize,
}

/// Defines constant values for the memory address offsets
impl SerialInputOutputDevice {
    // Define memory size and offset values
    const DEVICE_MEM_SIZE: usize = 32;
    const OFFSET_INPUT_SIZE: usize = 0;
    const OFFSET_INPUT_GET: usize = 1;
    const OFFSET_OUTPUT_SIZE: usize = 2;
    const OFFSET_OUTPUT_SET: usize = 3;
    const OFFSET_INPUT_RESET_IN: usize = 4;
    const OFFSET_INPUT_RESET_OUT: usize = 5;

    /// Constructs a new serial device
    pub fn new(buffer_size: usize) -> SerialInputOutputDevice {
        // Construct the serial device output
        Self {
            input_queue: RefCell::new(VecDeque::new()),
            output_queue: VecDeque::new(),
            buffer_size,
        }
    }

    /// Determines if there is output in the queue
    pub fn has_output(&self) -> bool {
        !self.output_queue.is_empty()
    }

    /// Determines if there is input in the queue
    pub fn has_input(&self) -> bool {
        return !self.input_queue.borrow().is_empty();
    }

    /// Pushes the input value into the input queue
    pub fn push_input(&mut self, val: MemoryWord) -> bool {
        if self.input_queue.borrow().len() < self.buffer_size {
            self.input_queue.borrow_mut().push_back(val);
            true
        } else {
            false
        }
    }

    /// Pops the output value from the output queue and returns
    pub fn pop_output(&mut self) -> Option<MemoryWord> {
        self.output_queue.pop_front()
    }

    fn common_get(&self, offset: usize) -> Result<MemoryWord, SolariumError> {
        // Use the offset values to determine the action to take
        return match offset {
            Self::OFFSET_INPUT_SIZE => Ok(MemoryWord::new(self.input_queue.borrow().len() as u16)),
            Self::OFFSET_OUTPUT_SIZE => Ok(MemoryWord::new(self.output_queue.len() as u16)),
            Self::OFFSET_OUTPUT_SET => Ok(MemoryWord::new(0)),
            _ => Err(SolariumError::InvalidMemoryAccess(offset)),
        };
    }
}

impl MemorySegment for SerialInputOutputDevice {
    /// Provides the word at the requested memory location
    fn get(&self, offset: usize) -> Result<MemoryWord, SolariumError> {
        // Use the offset values to determine the action to take
        return match offset {
            Self::OFFSET_INPUT_GET => match self.input_queue.borrow_mut().pop_front() {
                Some(v) => Ok(v),
                None => Ok(MemoryWord::new(0)),
            },
            _ => self.common_get(offset),
        };
    }

    /// Provides the word at the requested memory location without affecting the device state
    fn inspect(&self, offset: usize) -> Result<MemoryWord, SolariumError> {
        // Use the offset values to determine the action to take
        return match offset {
            Self::OFFSET_INPUT_GET => match self.input_queue.borrow().front() {
                Some(v) => Ok(*v),
                None => Ok(MemoryWord::new(0)),
            },
            _ => self.common_get(offset),
        };
    }

    /// Sets the word at the requested memory location with the given data
    /// Returns true if the value could be set; otherwise returns false
    fn set(&mut self, offset: usize, data: MemoryWord) -> Result<(), SolariumError> {
        // Return error if not within the given offset value
        if !self.within(offset) {
            return Err(SolariumError::InvalidMemoryAccess(offset));
        }

        // Extract the offset and match based on the result
        return match offset {
            Self::OFFSET_OUTPUT_SET => {
                if self.output_queue.len() < self.buffer_size {
                    self.output_queue.push_back(data);
                    Ok(())
                } else {
                    Err(SolariumError::DeviceError(
                        0,
                        SolariumDeviceError::BufferFull,
                    ))
                }
            }
            Self::OFFSET_INPUT_RESET_IN => {
                if data.get() != 0 {
                    self.input_queue.borrow_mut().clear();
                }
                Ok(())
            }
            Self::OFFSET_INPUT_RESET_OUT => {
                if data.get() != 0 {
                    self.output_queue.clear();
                }
                Ok(())
            }
            _ => Err(SolariumError::InvalidMemoryWrite(offset)),
        };
    }

    /// Resets the memory segment
    fn reset(&mut self) {
        self.input_queue.borrow_mut().clear();
        self.output_queue.clear();
    }

    /// Provides the length of the memory segment
    fn len(&self) -> usize {
        Self::DEVICE_MEM_SIZE
    }
}

impl SolariumDevice for SerialInputOutputDevice {
    fn on_step(&mut self) -> Option<DeviceAction> {
        None
    }
}
