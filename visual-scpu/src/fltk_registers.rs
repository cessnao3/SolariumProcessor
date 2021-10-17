use fltk::prelude::*;

use fltk::enums::Align;
use fltk::text::{TextDisplay, TextBuffer};
use fltk::group::Flex;
use fltk::frame::Frame;

use libscpu::cpu::SolariumCPU;

pub fn setup_register_group(parent: &mut Flex) -> Vec<TextDisplay>
{
    let mut column_group = Flex::default_fill().column();

    let mut displays = Vec::new();

    assert!(SolariumCPU::NUM_REGISTERS % 2 == 0);

    for i in 0..SolariumCPU::NUM_REGISTERS / 2
    {
        let v = add_register_row(&mut column_group, i*2, i*2 + 1);
        for disp in v
        {
            displays.push(disp);
        }
    }

    column_group.end();

    parent.set_size(&mut column_group, 280);

    return displays;
}

fn add_register_row(parent: &mut Flex, reg0_ind: usize, reg1_ind: usize) -> Vec<TextDisplay>
{
    let mut displays = Vec::new();
    let mut row_group = Flex::default_fill().row();

    displays.push(setup_register_display(&mut row_group, reg0_ind));
    displays.push(setup_register_display(&mut row_group, reg1_ind));

    row_group.end();

    parent.set_size(&mut row_group, 30);

    return displays;
}

fn setup_register_display(parent: &mut Flex, index: usize) -> TextDisplay
{
    let mut label = Frame::default().with_label(&format!("R{0:}", index));
    let mut text_display = TextDisplay::default().with_align(Align::BottomRight);

    let buffer = TextBuffer::default();
    text_display.set_buffer(buffer);

    parent.set_size(&mut label, 50);
    parent.set_size(&mut text_display, 100);

    return text_display;
}
