
fn bar(v: Vec<& i32>) { }
fn main() { let mut x = 42; bar(vec![&mut x]); }
