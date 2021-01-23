fn compare(x: i32, y: i32) {
    if x < y { print!("<") }
    else if x > y { print!(">") }
    else { print!("=") };
}
fn print(v: &Vec<i32>) {
    compare(v[0], v[1]);
    compare(v[1], v[2]);
    compare(v[2], v[3]);
    print!("\n");
}
fn main() {
    let mut v = vec![0, 1, 2, 3];
    print(&v);
    v[2] = v[1];
    print(&v);
    v[2] = 42;
    print(&v);
    if v.len() == 4 { print!("len() OK\n") } else { print!("bad len()\n") }
}

