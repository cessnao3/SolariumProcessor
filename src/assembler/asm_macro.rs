#[macro_export]
macro_rules! match_err
{
    ($a: expr, $l: expr) =>
    {
        // Match the input value argument
        match $a
        {
            Ok(v) => v,
            Err(e) => return Err(format!("Line {0:} error: {1:}", $l, e))
        };
    }
}
