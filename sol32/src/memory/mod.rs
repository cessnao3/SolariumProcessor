
mod segment_ro;
mod segment_rw;

use std::cell::RefCell;

use crate::common::Word;

pub enum MemoryError {
    InvalidMemoryAccess(u32),
    ReadOnlyMemory(u32),
    OverlappingSegment(u32),
    EmptySegment(u32),
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

    /// Determines whether the memory map is empty
    fn is_empty(&self) -> bool {
        self.len() == 0
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
        else if new_seg.seg.borrow().is_empty() {
            return Err(MemoryError::EmptySegment(base));
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

    pub fn reset(&mut self) {
        for s in self.segments.iter() {
            s.seg.borrow_mut().reset();
        }
    }

    pub fn get_f32(&mut self, address: u32) -> Result<f32, MemoryError> {
        Ok(f32::from_bits(self.get_u32(address)?))
    }

    pub fn set_f32(&mut self, address: u32, val: f32) -> Result<(), MemoryError> {
        self.set_u32(address, val.to_bits())
    }

    pub fn get_u32(&mut self, address: u32) -> Result<u32, MemoryError> {
        let mut val = 0;
        for i in 0..4 {
            val |= (self.get_word(address + i)?.get() as u32) << (8 * i);
        }
        Ok(val)
    }

    pub fn set_u32(&mut self, address: u32, val: u32) -> Result<(), MemoryError> {
        let mut val = val;
        for i in 0..4 {
            self.set_word(address + i, Word::from((val & 0xFF) as u8))?;
            val >>= 8;
        }
        Ok(())
    }

    pub fn get_u16(&mut self, address: u32) -> Result<u16, MemoryError> {
        let mut val = 0;
        for i in 0..2 {
            val |= (self.get_word(address)?.get() as u16) << (8 * i);
        }
        Ok(val)
    }

    pub fn set_u16(&mut self, address: u32, val: u16) -> Result<(), MemoryError> {
        let mut val = val;
        for i in 0..2 {
            self.set_word(address + i, Word::from((val & 0xFF) as u8))?;
            val >>= 8;
        }
        Ok(())
    }
}
