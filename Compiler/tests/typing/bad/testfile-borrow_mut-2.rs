
fn main() { let mut x : i32 = 41; { let y : &i32 = &x; *y = *y + 1; } return; }
