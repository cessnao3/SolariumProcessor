use alloc::collections::VecDeque;
use core::cell::RefCell;

use super::{ProcessorDevice, DEVICE_ID_SIZE, DEVICE_MEM_SIZE};

use crate::memory::{MemorySegment, MemorySegmentError};

/// Provides a memory serial I/O memory-mapped device
pub struct SerialInputOutputDevice {
    /// Provides the base address for the input device
    input_queue: RefCell<VecDeque<u8>>,
    output_queue: VecDeque<u8>,
    buffer_size: usize,
}

/// Defines constant values for the memory address offsets
impl SerialInputOutputDevice {
    // Define memory size and offset values
    const OFFSET_INPUT_SIZE: u32 = 2;
    const OFFSET_INPUT_GET: u32 = 3;
    const OFFSET_OUTPUT_SIZE: u32 = 4;
    const OFFSET_OUTPUT_SET: u32 = 5;
    const OFFSET_INPUT_RESET_IN: u32 = 6;
    const OFFSET_INPUT_RESET_OUT: u32 = 7;

    const DEVICE_ID: u16 = 1;

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
    pub fn push_input(&mut self, val: u8) -> bool {
        if self.input_queue.borrow().len() < self.buffer_size {
            self.input_queue.borrow_mut().push_back(val);
            true
        } else {
            false
        }
    }

    /// Pops the output value from the output queue and returns
    pub fn pop_output(&mut self) -> Option<u8> {
        self.output_queue.pop_front()
    }

    fn common_get(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        // Use the offset values to determine the action to take
        match offset {
            n if n < DEVICE_ID_SIZE => Ok(Self::DEVICE_ID.to_be_bytes()[offset as usize]),
            Self::OFFSET_INPUT_SIZE => {
                Ok((u8::MAX as usize).min(self.input_queue.borrow().len()) as u8)
            }
            Self::OFFSET_OUTPUT_SIZE => Ok((u8::MAX as usize).min(self.output_queue.len()) as u8),
            Self::OFFSET_OUTPUT_SET => Ok(0),
            _ => Err(MemorySegmentError::InvalidMemoryAccess(offset)),
        }
    }
}

impl MemorySegment for SerialInputOutputDevice {
    /// Provides the word at the requested memory location
    fn get(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        // Use the offset values to determine the action to take
        match offset {
            Self::OFFSET_INPUT_GET => match self.input_queue.borrow_mut().pop_front() {
                Some(v) => Ok(v),
                None => Ok(0),
            },
            _ => self.common_get(offset),
        }
    }

    /// Provides the word at the requested memory location without affecting the device state
    fn inspect(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        // Use the offset values to determine the action to take
        match offset {
            Self::OFFSET_INPUT_GET => match self.input_queue.borrow().front() {
                Some(v) => Ok(*v),
                None => Ok(0),
            },
            _ => self.common_get(offset),
        }
    }

    /// Sets the word at the requested memory location with the given data
    /// Returns true if the value could be set; otherwise returns false
    fn set(&mut self, offset: u32, data: u8) -> Result<(), MemorySegmentError> {
        // Return error if not within the given offset value
        if !self.within(offset) {
            return Err(MemorySegmentError::InvalidMemoryAccess(offset));
        }

        // Extract the offset and match based on the result
        match offset {
            Self::OFFSET_OUTPUT_SET => {
                if self.output_queue.len() < self.buffer_size {
                    self.output_queue.push_back(data);
                    Ok(())
                } else {
                    Err(MemorySegmentError::InvalidMemoryWrite(offset, data))
                }
            }
            Self::OFFSET_INPUT_RESET_IN => {
                if data != 0 {
                    self.input_queue.borrow_mut().clear();
                }
                Ok(())
            }
            Self::OFFSET_INPUT_RESET_OUT => {
                if data != 0 {
                    self.output_queue.clear();
                }
                Ok(())
            }
            _ => Err(MemorySegmentError::InvalidMemoryWrite(offset, data)),
        }
    }

    /// Resets the memory segment
    fn reset(&mut self) {
        self.input_queue.borrow_mut().clear();
        self.output_queue.clear();
    }

    /// Provides the length of the memory segment
    fn len(&self) -> u32 {
        DEVICE_MEM_SIZE
    }
}

impl ProcessorDevice for SerialInputOutputDevice {
    fn device_id(&self) -> u16 {
        Self::DEVICE_ID
    }
}
