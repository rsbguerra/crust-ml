fn print_bool(b: bool) {
    if b { print!("true\n") } else { print!("false\n") }
}

fn main() {
    print_bool(true);
    print_bool(false);
    print_bool(true  && true);
    print_bool(true  && false);
    print_bool(false && true);
    print_bool(false && false);
    print_bool(true  || true);
    print_bool(true  || false);
    print_bool(false || true);
    print_bool(false || false);
    print_bool(!true);
    print_bool(!false);
    if 1+2 == 3 || 1/0 == 0 { print!("yes\n") }
    if 1+2 == 4 && 1/0 == 0 { print!("no\n") }
}
