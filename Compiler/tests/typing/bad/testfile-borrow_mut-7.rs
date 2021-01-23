
fn bar(v: Vec<&mut i32>) { }
fn main() { let x = 42; bar(vec![& x]); }
