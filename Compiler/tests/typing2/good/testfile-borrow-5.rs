
fn bar(v: Vec<& i32>) { }
fn main() { let mut x = 42; let y = 89; bar(vec![&y, &mut x]); }

