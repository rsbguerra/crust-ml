struct L {
    head: i32
}

fn make() -> L {
    let r = L { head: 42 };
    r
}

fn main() {
    let s = make();
    if s.head == 42 { print!("yes!\n") } else { print!("oups!\n") }
}

