fn test(x: i32, r: i32) {
    if x == r { print!("yes\n") } else { print!("no\n") }
}
fn main() {
    test(1+2, 3);
    test(100+2*4, 108);
    test(5/2, 3);
    test(-5/2, -2);
    test(5%2, 1);
    test(-5%2, 1);
}
