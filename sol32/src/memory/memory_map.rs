use super::{MemorySegment, MemoryError, Word, MemorySegmentError};

use std::{cell::RefCell, mem::size_of};

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

fn segment_to_memory<T>(seg: &SegmentData, result: Result<T, MemorySegmentError>) -> Result<T, MemoryError> {
    match result {
        Err(MemorySegmentError::InvalidMemoryAccess(offset)) => Err(MemoryError::InvalidMemoryAccess(seg.base + offset)),
        Err(MemorySegmentError::ReadOnlyMemory(offset)) => Err(MemoryError::ReadOnlyMemory(seg.base + offset)),
        Ok(v) => Ok(v),
    }
}

pub struct MemoryMap {
    segments: Vec<SegmentData>,
}

macro_rules! GetSetUnsignedType {
    ( $get_name: ident, $set_name: ident, $type: ident ) => {
        pub fn $get_name(&mut self, address: u32) -> Result<$type, MemoryError> {
            let mut val: $type = 0;
            for i in 0..size_of::<$type>() {
                val |= (self.get_u8(address + i as u32)?.get() as $type) << (8 * (size_of::<$type>() - 1 - i));
            }
            Ok(val)
        }

        pub fn $set_name(&mut self, address: u32, mut val: $type) -> Result<(), MemoryError> {
            for i in 0..size_of::<$type>() {
                self.set_u8(address + size_of::<$type>() as u32 - 1 - i as u32, Word::from((val & 0xFF) as u8))?;
                val >>= 8;
            }
            Ok(())
        }
    };
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

    pub fn get_u8(&self, address: u32) -> Result<Word, MemoryError> {
        let data = self.get_segment(address)?;
        segment_to_memory(data, data.seg.borrow().get(address))
    }

    pub fn set_u8(&mut self, address: u32, val: Word) -> Result<(), MemoryError> {
        let data = self.get_segment(address)?;
        segment_to_memory(data, data.seg.borrow_mut().set(address, val))
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

    GetSetUnsignedType!(get_u32, set_u32, u32);
    GetSetUnsignedType!(get_u16, set_u16, u16);
}