fn main() { let x = 41; { let y = &mut x; *y = *y + 1; } }
