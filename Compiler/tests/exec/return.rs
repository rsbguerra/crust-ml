fn f1() -> i32 {
    return 1;
    return 2;
    3
}
fn f2() -> i32 {
    if 1 < { return 2; 3 } { print!("oups"); 4 } else { print!("oups"); 5 }
}
fn f3() -> i32 {
    while true {
        return 3;
    }
    4
}
fn f(x: i32) -> i32 {
    if x == 0 { return 0; }
    return 1 + { return x; 2 };
}

fn main() {
    if f1() == 1 { print!("yes\n"); }
    if f2() == 2 { print!("yes\n"); }
    if f3() == 3 { print!("yes\n"); }
    if f(0) == 0 { print!("f(0) = 0\n"); } else { print!("f(0) = ???\n"); }
    if f(1) == 1 { print!("f(1) = 1\n"); } else { print!("f(1) = ???\n"); }
    return;
    print!("oups");
}

