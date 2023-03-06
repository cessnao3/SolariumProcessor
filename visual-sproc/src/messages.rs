use sproc::common::MemoryWord;

use super::processor_state::RegisterArray;

#[derive(Clone, Copy)]
pub enum FltkMessage {
    Step,
    Start,
    Stop,
    Reset,
    Assemble,
    Compile,
    CompileToText,
    Tick,
    SetSpeed(f64),
    SerialInput,
    HardwareInterrupt(usize),
    FileLoadError,
}

#[derive(Clone)]
pub enum ThreadMessage {
    SetMemory(Vec<MemoryWord>),
    Start,
    Stop,
    Reset,
    Step,
    SetSpeed(f64),
    SerialInput(Vec<char>),
    HardwareInterrupt(usize),
}

#[derive(Clone)]
pub enum GuiMessage {
    UpdateRegisters(RegisterArray),
    UpdateMemory(Vec<MemoryWord>),
    SerialOutput(char),
    LogMessage(String),
}
