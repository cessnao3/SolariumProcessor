use sproc::common::MemoryWord;

use super::processor_state::RegisterArray;

#[derive(Clone, Copy)]
pub struct SerialBuffer
{
    pub ptr: *mut Vec<char>
}
unsafe impl Send for SerialBuffer {}
unsafe impl Sync for SerialBuffer {}

impl SerialBuffer
{
    pub fn from_box(b: Box<Vec<char>>) -> SerialBuffer
    {
        return Self
        {
            ptr: Box::into_raw(b)
        };
    }
}

#[derive(Clone, Copy)]
pub enum FltkMessage
{
    Step,
    Start,
    Stop,
    Reset,
    Assemble,
    Tick,
    SetSpeed(f64),
    SerialInput(SerialBuffer),
    HardwareInterrupt(usize),
    FileLoadError
}

#[derive(Clone)]
pub enum ThreadMessage
{
    SetMemory(Vec<MemoryWord>),
    Start,
    Stop,
    Reset,
    Step,
    SetSpeed(f64),
    SerialInput(Box<Vec<char>>),
    HardwareInterrupt(usize)
}

#[derive(Clone)]
pub enum GuiMessage
{
    UpdateRegisters(RegisterArray),
    UpdateMemory(Vec<MemoryWord>),
    SerialOutput(char),
    LogMessage(String)
}
