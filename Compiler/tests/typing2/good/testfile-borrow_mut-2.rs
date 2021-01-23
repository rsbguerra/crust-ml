
fn main() { let mut x = 41; { let y = &mut x; *y = *y + 1; } }
