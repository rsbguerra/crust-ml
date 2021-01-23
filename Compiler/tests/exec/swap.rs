fn swap(x: &mut i32, y: &mut i32) {
    let t = *x;
    *x = *y;
    *y = t;
}

fn compare(x: i32, y: i32) {
    if x < y { print!("-") }
    else if x > y { print!("+") }
    else { print!("=") };
    print!("\n");
}

fn main() {
    let mut x = 41;
    let mut y = 42;
    compare(x, y);
    swap(&mut x, &mut y);
    compare(x, y);
}

