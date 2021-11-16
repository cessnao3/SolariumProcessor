use std::cell::RefCell;
use std::collections::VecDeque;

use crate::memory::{MemorySegment, MEM_MAX_SIZE};
use crate::common::{MemoryWord, SolariumError};

/// Provides a memory serial I/O memory-mapped device
pub struct SerialInputOutputDevice
{
    /// Provides the base address for the input device
    base_address: usize,
    input_queue: RefCell<VecDeque<MemoryWord>>,
    output_queue: VecDeque<MemoryWord>
}

/// Defines constant values for the memory address offsets
impl SerialInputOutputDevice
{
    const DEVICE_MEM_SIZE: usize = 16;
    const OFFSET_INPUT_SIZE: usize = 0;
    const OFFSET_INPUT_GET: usize = 1;
    const OFFSET_OUTPUT_SIZE: usize = 2;
    const OFFSET_OUTPUT_SET: usize = 3;

    /// Constructs a new serial device
    pub fn new(base_address: usize) -> SerialInputOutputDevice
    {
        // Define the top address
        let top_address = base_address + Self::DEVICE_MEM_SIZE;

        // Assert that the memory values are okay and within bounds
        assert!(top_address > base_address);
        assert!(top_address <= MEM_MAX_SIZE);

        // Construct the serial device output
        return Self
        {
            base_address,
            input_queue: RefCell::new(VecDeque::new()),
            output_queue: VecDeque::new()
        };
    }
}

impl MemorySegment for SerialInputOutputDevice
{
    /// Provides the word at the requested memory location
    fn get(&self, ind: usize) -> Result<MemoryWord, SolariumError>
    {
        // Return error if not within the selected index
        if !self.within(ind)
        {
            return Err(SolariumError::InvalidMemoryAccess(ind));
        }

        // Determine the base offset value
        let offset = ind - self.base_address;

        // Use the offset values to determine the action to take
        return match offset
        {
            Self::OFFSET_INPUT_SIZE => Ok(MemoryWord::new(self.input_queue.borrow().len() as u16)),
            Self::OFFSET_INPUT_GET =>
            {
                match self.input_queue.borrow_mut().pop_front()
                {
                    Some(v) => Ok(v),
                    None => Ok(MemoryWord::new(0))
                }
            },
            Self::OFFSET_OUTPUT_SIZE => Ok(MemoryWord::new(self.output_queue.len() as u16)),
            Self::OFFSET_OUTPUT_SET => Ok(MemoryWord::new(0)),
            _ => Err(SolariumError::InvalidMemoryAccess(ind))
        };
    }

    /// Sets the word at the requested memory location with the given data
    /// Returns true if the value could be set; otherwise returns false
    fn set(&mut self, ind: usize, data: MemoryWord) -> Result<(), SolariumError>
    {
        // Return error if not within the given offset value
        if !self.within(ind)
        {
            return Err(SolariumError::InvalidMemoryAccess(ind))
        }

        // Extract the offset and match based on the result
        let offset = ind - self.base_address;
        return match offset
        {
            Self::OFFSET_OUTPUT_SET =>
            {
                self.output_queue.push_back(data);
                Ok(())
            },
            _ => Err(SolariumError::InvalidMemoryWrite(ind))
        }
    }

    /// Resets the memory segment
    fn reset(&mut self)
    {
        self.input_queue.borrow_mut().clear();
        self.output_queue.clear();
    }

    /// Provides the starting address of the memory segment
    fn start_address(&self) -> usize
    {
        return self.base_address;
    }

    /// Provides the length of the memory segment
    fn address_len(&self) -> usize
    {
        return Self::DEVICE_MEM_SIZE;
    }

    /// Determines if the given memory index is within the memory segment
    fn within(&self, ind: usize) -> bool
    {
        return ind >= self.base_address && ind < self.base_address + self.address_len();
    }
}
