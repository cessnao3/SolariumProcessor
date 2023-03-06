mod memory_map;
mod segment_ro;
mod segment_rw;

use crate::common::{MemoryWord, SolariumError};

pub use self::memory_map::MemoryMap;
pub use self::segment_ro::ReadOnlySegment;
pub use self::segment_rw::ReadWriteSegment;

/// Define the maximum possible size
pub const BITS_PER_WORD: usize = 16usize;
pub const MEM_MAX_SIZE: usize = (2usize).pow(BITS_PER_WORD as u32);

/// Provides a trait for memory segments to implement
pub trait MemorySegment {
    /// Provides the word at the requested memory location
    fn get(&self, offset: usize) -> Result<MemoryWord, SolariumError>;

    /// Provides the word at the requested memory location without modifying the device state
    fn inspect(&self, offset: usize) -> Result<MemoryWord, SolariumError>;

    /// Sets the word at the requested memory location with the given data
    /// Returns true if the value could be set; otherwise returns false
    fn set(&mut self, offset: usize, data: MemoryWord) -> Result<(), SolariumError>;

    /// Resets the memory segment
    fn reset(&mut self);

    /// Provides the length of the memory segment
    fn len(&self) -> usize;

    /// Returns true if the memory segment has 0 length
    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Determines if the given memory index is within the memory segment
    fn within(&self, offset: usize) -> bool {
        offset < self.len()
    }
}
