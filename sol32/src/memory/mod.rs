
mod segment_ro;
mod segment_rw;

use std::cell::RefCell;

use crate::common::Word;

pub enum MemoryError {
    InvalidMemoryAccess(u32),
    ReadOnlyMemory(u32),
    OverlappingSegment(u32),
    InvalidAddress(u32),
    IndexBounds(usize),
}

pub trait MemorySegment {
    /// Provides the word at the requested memory location
    fn get(&self, offset: u32) -> Result<Word, MemoryError>;

    /// Provides the word at the requested memory location without affecting the device state
    fn inspect(&self, offset: u32) -> Result<Word, MemoryError> {
        self.get(offset)
    }

    /// Sets the word at the requested memory location with the given data
    /// Returns true if the value could be set; otherwise returns false
    fn set(&mut self, offset: u32, val: Word) -> Result<(), MemoryError>;

    /// Provides the length of the memory segment
    fn len(&self) -> u32;

    /// Resets the memory segment
    fn reset(&mut self);

    /// Determines that the offset is within the memory segment
    fn within(&self, offset: u32) -> bool {
        offset < self.len()
    }
}

struct SegmentData {
    base: u32,
    seg: Box<RefCell<dyn MemorySegment>>,
}

impl SegmentData {
    pub fn within(&self, addr: u32) -> bool {
        addr >= self.base && addr < self.top()
    }

    pub fn top(&self) -> u32 {
        self.base + self.seg.borrow().len()
    }
}

pub struct MemoryMap {
    segments: Vec<SegmentData>,
}

impl MemoryMap {
    pub fn add_segment(&mut self, base: u32, seg: Box<RefCell<dyn MemorySegment>>) -> Result<(), MemoryError> {
        let new_seg = SegmentData { base, seg };

        let top = base as usize + new_seg.seg.borrow().len() as usize;
        if top > u32::MAX as usize {
            return Err(MemoryError::IndexBounds(top));
        }

        for sd in self.segments.iter() {
            if new_seg.within(sd.base) || new_seg.within(sd.top()) || sd.within(new_seg.base) || sd.within(new_seg.top()) {
                return Err(MemoryError::OverlappingSegment(base));
            }
        }

        self.segments.push(new_seg);
        Ok(())
    }

    pub fn get_word(&self, address: u32) -> Result<Word, MemoryError> {
        let data = self.get_segment(address)?;
        data.seg.borrow().get(address)
    }

    pub fn set_word(&mut self, address: u32, val: Word) -> Result<(), MemoryError> {
        let data = self.get_segment(address)?;
        data.seg.borrow_mut().set(address, val)
    }

    fn get_segment(&self, address: u32) -> Result<&SegmentData, MemoryError> {
        for m in self.segments.iter() {
            if m.within(address) {
                return Ok(m);
            }
        }

        Err(MemoryError::InvalidAddress(address))
    }
}
