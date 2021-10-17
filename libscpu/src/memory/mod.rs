mod memory_map;
mod segment_rw;

pub use self::memory_map::MemoryMap as MemoryMap;
pub use self::segment_rw::ReadWriteSegment as ReadWriteSegment;

/// Provides the data type to use for a word in memory
pub type MemoryWord = u16;

/// Provides the corresponding signed type for the type used for [MemoryWord]
pub type MemoryWordSigned = i16;

/// Define the maximum possible size
pub const MAX_SEGMENT_SIZE: usize = (2usize).pow(16);

/// Provides a trait for memory segments to implement
pub trait MemorySegment
{
    /// Provides the word at the requested memory location
    fn get(&self, ind: usize) -> MemoryWord;

    /// Sets the word at the requested memory location with the given data
    /// Returns true if the value could be set; otherwise returns false
    fn set(&mut self, ind: usize, data: MemoryWord) -> bool;

    /// Resets the memory segment
    fn reset(&mut self);

    /// Provides the starting address of the memory segment
    fn start_address(&self) -> usize;

    /// Provides the length of the memory segment
    fn address_len(&self) -> usize;

    /// Determines if the given memory index is within the memory segment
    fn within(&self, ind: usize) -> bool;
}
