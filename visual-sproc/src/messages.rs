use sproc::common::MemoryWord;
use sproc::cpu::SolariumProcessor;

#[derive(Clone)]
pub enum UiToThread {
    CpuStep,
    CpuStart,
    CpuStop,
    CpuReset,
    CpuIrq(u8),
    SetCode(Vec<MemoryWord>),
    SerialInput(String),
    RequestMemory(usize, usize),
    SetMultiplier(f64),
    Exit,
}

#[derive(Clone)]
pub enum ThreadToUi {
    ResponseMemory(usize, Vec<MemoryWord>),
    SerialOutput(String),
    LogMessage(String),
    RegisterState([MemoryWord; SolariumProcessor::NUM_REGISTERS]),
    ProcessorReset,
    ThreadExit,
}
