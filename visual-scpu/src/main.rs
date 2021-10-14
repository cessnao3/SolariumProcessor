use fltk::prelude::*;
use fltk::{app, button::Button, frame::Frame, window::Window};

fn main()
{
    // Define the window values
    let app = app::App::default();
    //let mut wind = Window::new(100, 100, 400, 300, "Hello from rust");
    let mut wind = Window::default()
        .with_size(700, 600)
        .with_label("VisualSCPU");
    let mut frame = Frame::new(0, 0, 400, 200, "").set_color(Color::blu);
    let mut but = Button::new(160, 210, 80, 40, "Click me!");
    wind.end();
    wind.show();
    but.set_callback(move |_| frame.set_label("Hello World!")); // the closure capture is mutable borrow to our button
    app.run().unwrap();
}
