
fn main() { let mut x = 41; { let y = & x; *y = *y + 1; } }
