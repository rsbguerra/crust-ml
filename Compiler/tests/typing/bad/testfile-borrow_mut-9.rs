
fn bar(v: Vec<&mut i32>) {  return; }
fn main() { let mut x: i32 = 42; bar(vec![& x]);  return; }
