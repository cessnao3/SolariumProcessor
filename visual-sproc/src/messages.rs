use libsproc::common::MemoryWord;

use super::processor_state::RegisterArray;

pub const CHAR_BUF_SIZE: usize = 256;
pub type SerialCharBuf = [char; 256];

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
    SerialInput(SerialCharBuf),
    HardwareInterrupt(usize)
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
    SerialInput(SerialCharBuf),
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
