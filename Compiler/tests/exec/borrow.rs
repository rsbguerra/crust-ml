
fn foo(x: &i32) {
    if      *x == 0 { print!("0\n"); }
    else if *x < 0  { print!("-\n"); }
    else if *x > 0  { print!("+\n"); }
    else            { print!("absurd!\n"); 1/0; }
}

fn bar(x: &mut i32) {
    foo(x);
    *x = *x - 2;
    foo(x);
}

fn main() {
    let mut x = 8;
    {
        let y = &mut x;
        *y = *y + 1;
    };
    foo(&x);
    if x == 9 { print!("yes\n"); } else { print!("oups\n"); }
    while x > 0 {
        bar(&mut x);
    }
    if x == -1 { print!("yes\n"); } else { print!("oups\n"); }
}
