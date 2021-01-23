fn main() { let x : i32 = 41; { let y: &mut i32 = &mut x; *y = *y + 1;} return; }
