mod memory_map;
mod segment_ro;
mod segment_rw;

pub use segment_ro::ReadOnlySegment;
pub use segment_rw::ReadWriteSegment;
pub use memory_map::MemoryMap;

/// Provides error conditions for memory segment parameters
pub enum MemoryError {
    InvalidMemoryAccess(u32),
    ReadOnlyMemory(u32),
    OverlappingSegment(u32),
    EmptySegment(u32),
    InvalidAddress(u32),
    IndexBounds(usize),
}

pub enum MemorySegmentError {
    InvalidMemoryAccess(u32),
    ReadOnlyMemory(u32),
}

pub trait MemorySegment {
    /// Provides the word at the requested memory location
    fn get(&self, offset: u32) -> Result<u8, MemorySegmentError>;

    /// Provides the word at the requested memory location without affecting the device state
    fn inspect(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        self.get(offset)
    }

    /// Sets the word at the requested memory location with the given data
    /// Returns true if the value could be set; otherwise returns false
    fn set(&mut self, offset: u32, val: u8) -> Result<(), MemorySegmentError>;

    /// Provides the length of the memory segment
    fn len(&self) -> u32;

    /// Resets the memory segment
    fn reset(&mut self);

    /// Determines that the offset is within the memory segment
    fn within(&self, offset: u32) -> bool {
        offset < self.len()
    }

    /// Determines whether the memory map is empty
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
