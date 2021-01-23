/* arbres binaires de recherche */

struct BST {
  value: i32,
  sub: Vec<BST> // de taille 0 ou 2
}

fn null() -> BST {
    let r = BST { value: 42, sub: vec![] };
    r
}

fn is_null(a: & BST) -> bool {
    a.sub.len() == 0
}

// Petit Rust ne permet pas de définir ces raccourcis ; tant pis...

// fn left(a: & BST) -> & BST { & a.sub[0] }
// fn right(a: & BST) -> & BST { & a.sub[1] }

// fn left_mut(a: &mut BST) -> &mut BST { &mut a.sub[0] }
// fn right_mut(a: &mut BST) -> &mut BST { &mut a.sub[1] }

fn leaf(v: i32) -> BST {
    let r = BST { value: v, sub: vec![null(), null()] };
    r
}

fn insert(a: &mut BST, x: i32) {
  if x == a.value { return; }
  if x < a.value {
    if is_null(& a.sub[0])
      { a.sub[0] = leaf(x); }
    else
      { insert(&mut a.sub[0], x); }
  } else {
    if is_null(& a.sub[1])
      { a.sub[1] = leaf(x); }
    else
      { insert(&mut a.sub[1], x); }
  }
}

fn contient(a: & BST, x: i32) -> bool {
  if x == a.value { return true; }
  if x < a.value && !is_null(& a.sub[0]) { return contient(& a.sub[0], x); }
  if !is_null(& a.sub[1]) { return contient(& a.sub[1], x); }
  return false;
}

fn print_bool(b: bool) {
    if b { print!("true\n") } else { print!("false\n") }
}

fn print_int(x: i32) {
    if x < 0 { print!("-") }
    else if x > 0 { print!("+") }
    else { print!("0") }
}

fn print(a: & BST) {
    print!("(");
    if !is_null(& a.sub[0]) { print(& a.sub[0]) }
    print_int(a.value);
    if !is_null(& a.sub[1]) { print(& a.sub[1]) }
    print!(")");
}

fn main() {
    let mut dict = leaf(1);
    let d = &mut dict;
    insert(d, 17);
    insert(d, -5);
    insert(d, 8);
    print_bool(contient(d, -5));
    print_bool(contient(d, 0));
    print_bool(contient(d, 17));
    print_bool(contient(d, 3));
    insert(d, 42);
    insert(d, 8);
    insert(d, -1000);
    insert(d, 0);
    print(d); print!("\n")
}
