#[derive(Clone)]
pub enum UiToThread {
    CpuStep,
    CpuStart,
    CpuStop,
    CpuReset,
    CpuIrq(u16),
    SetCode(Vec<u16>),
    SerialInput(String),
    RequestMemory(u16, u16),
    SetMultiplier(f64),
    Exit,
}

#[derive(Clone)]
pub enum ThreadToUi {
    ResponseMemory(u16, Vec<u16>),
    SerialOutput(String),
    LogMessage(String),
    RegisterState([u16; 16]),
    ThreadExit,
}
