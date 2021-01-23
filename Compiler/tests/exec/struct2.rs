struct S {
    a: i32, b: bool
}

fn main() {
    let s = S { a: 42, b: true };
    if s.a == 42 { print!("yes!\n") } else { print!("oups!\n") }
    if s.b       { print!("true!\n") } else { print!("false!\n") }
    let mut t = S { a: 0, b: false };
    if t.a == 0 { print!("yes!\n") } else { print!("oups!\n") }
    if t.b       { print!("true!\n") } else { print!("false!\n") }
    t = s;
    if t.a == 42 { print!("yes!\n") } else { print!("oups!\n") }
    if t.b       { print!("true!\n") } else { print!("false!\n") }
}
