
fn bar(v: Vec<&mut i32>) { return; }
fn main() { let x : i32 = 42; bar(vec![& x]); return; }
