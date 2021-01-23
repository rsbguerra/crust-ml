
struct L {
    head: i32,
    next: Vec<L>
}

fn print(x: i32) {
    if x == 42 { print!("=") } else
    if x  < 42 { print!("-") } else { print!("+") }
}

fn iter(l: &L) {
    print(l.head);
    if l.next.len() != 0 { iter(& l.next[0]); }
}

fn succ(l: &mut L) {
    l.head = l.head + 1;
    if l.next.len() != 0 { succ(&mut l.next[0]); }
}

fn append(l1: &mut L, l2: L) {
    if l1.next.len() == 0 { l1.next = vec![l2]; }
    else { append(&mut l1.next[0], l2); }
}

fn append2(mut l1: &mut L, l2: L) {
    // while l1.next.len() > 0 { l1 = &mut l1.next[0]; } // FAIL
    // while true { l1 = &mut l1.next[0]; } // FAIL
    while l1.next.len() > 0 { let x = l1; l1 = &mut x.next[0]; }
    l1.next = vec![l2];
}

fn main() {
    let mut l0 = L { head: 41, next: vec![] };
    let l1 = L { head: 42, next: vec![] };
    let mut l2 = L { head: 43, next: vec![l1] };
    let l4 = L { head: 40, next: vec![] };
    // iter(&l1);
    iter(&l2); print!("\n");
    succ(&mut l2);
    iter(&l2); print!("\n");
    // append(&mut l2, l2);
    append(&mut l0, l2);
    iter(&l0); print!("\n");
    append2(&mut l0, l4);
    iter(&l0); print!("\n");
}


