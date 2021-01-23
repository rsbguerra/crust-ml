
fn main() {
    let mut a = 41;
    let mut b = 42;
    let y = &mut b;
    y = &mut a;
    *y = 42;
}
