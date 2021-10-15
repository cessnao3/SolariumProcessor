use fltk::prelude::*;
use fltk::{app::*, button::*, dialog::*, enums::*, window::*, text::*, group::*, frame::*};

use libscpu::cpu::SolariumCPU;
use libscpu::memory::ReadWriteSegment;

#[derive(Clone, Copy)]
enum Message
{
    Step,
    UpdateRegisters
}

fn main()
{
    // Define the processor
    let mut cpu = SolariumCPU::new();
    cpu.memory_map.add_segment(Box::new(ReadWriteSegment::new(0, 10000)));

    // Define the window values
    let app = App::default();

    let (s, r) = channel::<Message>();

    //let mut wind = Window::new(100, 100, 400, 300, "Hello from rust");
    let mut wind = Window::default()
        .with_size(1100, 600)
        .with_label("VisualSCPU");

    let mut register_group = Flex::default_fill().column();

    // Add a simple step initiation button
    let mut button_group = Flex::default().row();
    let mut step_button = Button::default().with_label("STEP");
    step_button.emit(s, Message::Step);
    button_group.set_size(&mut step_button, 100);
    button_group.end();
    register_group.set_size(&mut button_group, 50);

    // Define the registers and perform the initial update values
    let register_labels = define_registers(&mut register_group);

    // End the register group
    register_group.end();

    // Finish Window Setup
    wind.end();

    // Init Parameters
    s.send(Message::UpdateRegisters);

    // Show the window and run the application
    wind.show();
    while app.wait()
    {
        if let Some(msg) = r.recv() {
            match msg {
                Message::Step =>
                {
                    match cpu.step()
                    {
                        Ok(_) =>
                        {
                            println!("Step Success");
                        }
                        Err(e) =>
                        {
                            alert_default(&format!("Step Error: {0:}", e));
                            println!("Step Error: {0:}", e)
                        }
                    }

                    s.send(Message::UpdateRegisters);
                },
                Message::UpdateRegisters =>
                {
                    for i in 0..SolariumCPU::NUM_REGISTERS
                    {
                        register_labels[i].buffer().unwrap().set_text(&format!("{0:}", cpu.get_register_value(i)));
                    }
                }
            }
        }
    }
}

fn define_registers(parent: &mut Flex) -> Vec<TextDisplay>
{
    let mut column_group = Flex::default_fill().column();

    let mut displays = Vec::new();

    for i in 0..8usize
    {
        let v = add_register_row(&mut column_group, i*2, i*2 + 1);
        for disp in v
        {
            displays.push(disp);
        }
    }

    column_group.end();

    parent.set_size(&mut column_group, 100);

    return displays;
}

fn add_register_row(parent: &mut Flex, reg0_ind: usize, reg1_ind: usize) -> Vec<TextDisplay>
{
    let mut displays = Vec::new();
    let mut row_group = Flex::default_fill().row();

    displays.push(setup_register(&mut row_group, reg0_ind));
    displays.push(setup_register(&mut row_group, reg1_ind));

    row_group.end();

    parent.set_size(&mut row_group, 30);

    return displays;
}

fn setup_register(parent: &mut Flex, index: usize) -> TextDisplay
{
    let mut label = Frame::default().with_label(&format!("R{0:}", index));
    let mut text_display = TextDisplay::default().with_align(Align::BottomRight);

    let buffer = TextBuffer::default();
    text_display.set_buffer(buffer);

    parent.set_size(&mut label, 40);
    parent.set_size(&mut text_display, 100);

    return text_display;
}
