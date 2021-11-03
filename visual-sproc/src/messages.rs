use libsproc::common::MemoryWord;

use super::processor_state::RegisterArray;

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
    HardwareInterrupt(usize)
}

#[derive(Clone)]
pub enum GuiMessage
{
    UpdateRegisters(RegisterArray),
    LogMessage(String)
}
