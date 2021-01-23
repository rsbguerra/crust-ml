
fn main() {
    let mut a : i32 = 41;
    let mut b : i32 = 42;
    let y : &mut i32 = &mut b;
    y = &mut a;
    *y = 42;
    return;
}
