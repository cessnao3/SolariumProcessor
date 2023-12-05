use sol32::cpu::RegisterManager;

#[derive(Clone)]
pub enum UiToThread {
    CpuStep,
    CpuStart,
    CpuStop,
    CpuReset,
    CpuIrq(u8),
    SetCode(Vec<u8>),
    SerialInput(String),
    RequestMemory(usize, usize),
    SetMultiplier(f64),
    Exit,
}

#[derive(Clone)]
pub enum ThreadToUi {
    ResponseMemory(usize, Vec<u8>),
    SerialOutput(String),
    LogMessage(String),
    RegisterState(RegisterManager),
    ProcessorReset,
    ThreadExit,
}
