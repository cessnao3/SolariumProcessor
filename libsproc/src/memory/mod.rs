mod memory_map;
mod segment_rw;
mod segment_ro;

use crate::common::{MemoryWord, SolariumError};

pub use self::memory_map::MemoryMap as MemoryMap;
pub use self::segment_rw::ReadWriteSegment as ReadWriteSegment;
pub use self::segment_ro::ReadOnlySegment as ReadOnlySegment;

/// Define the maximum possible size
pub const MAX_SEGMENT_INDEX: usize = (2usize).pow(16);

/// Provides a trait for memory segments to implement
pub trait MemorySegment
{
    /// Provides the word at the requested memory location
    fn get(&self, ind: usize) -> Result<MemoryWord, SolariumError>;

    /// Sets the word at the requested memory location with the given data
    /// Returns true if the value could be set; otherwise returns false
    fn set(&mut self, ind: usize, data: MemoryWord) -> Result<(), SolariumError>;

    /// Resets the memory segment
    fn reset(&mut self);

    /// Provides the starting address of the memory segment
    fn start_address(&self) -> usize;

    /// Provides the length of the memory segment
    fn address_len(&self) -> usize;

    /// Determines if the given memory index is within the memory segment
    fn within(&self, ind: usize) -> bool;
}
