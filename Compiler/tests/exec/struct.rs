struct S {
    i: i32,
    b: bool
}
fn print_bool(b: bool) {
    if b { print!("true") } else { print!("false") }
}

fn print_int(x: i32) {
    if x < 0 { print!("-") }
    else if x > 0 { print!("+") }
    else { print!("0") }
}

fn print_s(s: &S) {
    print!("[ ");
    print_int(s.i);
    print!(", ");
    print_bool(s.b);
    print!(" ]\n");
}

fn foo(s: &mut S) {
    print!("entering foo...\n");
    print_s(s);
    if s.b { s.i = - s.i; }
    s.b = ! s.b;
    print_s(s);
    print!("...leaving foo\n");
}

fn main() {
    let mut s = S { i: 42, b: true };
    foo(&mut s);
    foo(&mut s);
    foo(&mut s);
    foo(&mut s);
}
