
fn bar(v: Vec<&mut i32>) { }
fn main() { let mut x = 42; bar(vec![& x]); }
