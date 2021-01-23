
fn main() { let mut x = 40; let y = &mut x; *y = *y + 1; x = x + 1; }
