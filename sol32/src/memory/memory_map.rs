use super::{MemoryError, MemorySegment, MemorySegmentError};

use std::{cell::RefCell, mem::size_of, rc::Rc};

struct SegmentData {
    base: u32,
    seg: Rc<RefCell<dyn MemorySegment>>,
}

impl SegmentData {
    pub fn within(&self, addr: u32) -> bool {
        addr >= self.base && addr < self.top()
    }

    pub fn top(&self) -> u32 {
        self.base + self.seg.borrow().len()
    }

    pub fn get(&self, addr: u32) -> Result<u8, MemoryError> {
        let offset = addr - self.base;
        let res = self.seg.borrow().get(offset);
        self.segment_to_memory(res)
    }

    pub fn set(&self, addr: u32, val: u8) -> Result<(), MemoryError> {
        let offset = addr - self.base;
        let res = self.seg.borrow_mut().set(offset, val);
        self.segment_to_memory(res)
    }

    pub fn inspect(&self, addr: u32) -> Result<u8, MemoryError> {
        let offset = addr - self.base;
        let res = self.seg.borrow().inspect(offset);
        self.segment_to_memory(res)
    }

    fn segment_to_memory<T>(
        &self,
        result: Result<T, MemorySegmentError>,
    ) -> Result<T, MemoryError> {
        match result {
            Err(MemorySegmentError::InvalidMemoryAccess(offset)) => {
                Err(MemoryError::InvalidMemoryAccess(self.base + offset))
            }
            Err(MemorySegmentError::ReadOnlyMemory(offset)) => {
                Err(MemoryError::ReadOnlyMemory(self.base + offset))
            }
            Err(MemorySegmentError::InvalidMemoryWrite(offset, data)) => {
                Err(MemoryError::InvalidMemoryWrite(self.base + offset, data))
            }
            Ok(v) => Ok(v),
        }
    }
}

pub struct MemoryMap {
    segments: Vec<SegmentData>,
}

macro_rules! GetSetUnsignedType {
    ( $get_name: ident, $set_name: ident, $type: ident ) => {
        pub fn $get_name(&mut self, address: u32) -> Result<$type, MemoryError> {
            let mut bytes = [0; size_of::<$type>()];
            for i in 0..size_of::<$type>() {
                bytes[i] = self.get_u8(address + i as u32)?;
            }
            Ok($type::from_be_bytes(bytes))
        }

        pub fn $set_name(&mut self, address: u32, val: $type) -> Result<(), MemoryError> {
            for (i, v) in val.to_be_bytes().iter().enumerate() {
                self.set_u8(address + i as u32, *v)?;
            }
            Ok(())
        }
    };
}

impl MemoryMap {
    pub fn new() -> Self {
        MemoryMap {
            segments: Vec::new(),
        }
    }

    pub fn add_segment(
        &mut self,
        base: u32,
        seg: Rc<RefCell<dyn MemorySegment>>,
    ) -> Result<(), MemoryError> {
        let new_seg = SegmentData { base, seg };

        let top = base as usize + new_seg.seg.borrow().len() as usize;
        if top > u32::MAX as usize {
            return Err(MemoryError::IndexBounds(top));
        } else if new_seg.seg.borrow().is_empty() {
            return Err(MemoryError::EmptySegment(base));
        }

        for sd in self.segments.iter() {
            if new_seg.within(sd.base)
                || new_seg.within(sd.top() - 1)
                || sd.within(new_seg.base)
                || sd.within(new_seg.top() - 1)
            {
                return Err(MemoryError::OverlappingSegment(base));
            }
        }

        self.segments.push(new_seg);
        Ok(())
    }

    pub fn get_u8(&self, address: u32) -> Result<u8, MemoryError> {
        let data = self.get_segment(address)?;
        data.get(address)
    }

    pub fn inspect(&self, address: u32) -> Result<u8, MemoryError> {
        let data = self.get_segment(address)?;
        data.inspect(address)
    }

    pub fn set_u8(&mut self, address: u32, val: u8) -> Result<(), MemoryError> {
        let data = self.get_segment(address)?;
        data.set(address, val)
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

    GetSetUnsignedType!(get_u32, set_u32, u32);
    GetSetUnsignedType!(get_u16, set_u16, u16);
}

impl Default for MemoryMap {
    fn default() -> Self {
        Self::new()
    }
}
