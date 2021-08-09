pub mod memory_map;
pub mod segment_rw;

/// Provides the data type to use for a word in memory
pub type MemoryWord = u32;

/// Provides the corresponding signed type for the type used for [MemoryWord]
pub type MemoryWordSigned = i32;

/// Provides a trait for memory segments to implement
pub trait MemorySegment
{
    /// Provides the word at the requested memory location
    fn get(&self, ind: MemoryWord) -> MemoryWord;

    /// Sets the word at the requested memory location with the given data
    /// Returns true if the value could be set; otherwise returns false
    fn set(&mut self, ind: MemoryWord, data: MemoryWord) -> bool;

    /// Resets the memory segment
    fn reset(&mut self);

    /// Provides the starting address of the memory segment
    fn start_address(&self) -> MemoryWord;

    /// Provides the length of the memory segment
    fn address_len(&self) -> MemoryWord;

    /// Determines if the given memory index is within the memory segment
    fn within(&self, ind: MemoryWord) -> bool;
}
