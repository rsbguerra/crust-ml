
struct L {
    head: i32,
    next: Vec<L>
}

fn get(l: &L, i: i32) -> i32 {
    if i == 0 { l.head } else { get(&l.next[0], i-1) }
}

fn set(l: &mut L, i: i32, v: i32) {
    if i == 0 { l.head = v; } else { set(&mut l.next[0], i-1, v); }
}

fn make(n: i32) -> L {
    if n == 1 { let r = L { head:0, next: vec![] }; r }
    else { let r = L { head:0, next: vec![make(n-1)] }; r }
}

fn print_row(mut r: &L, i: i32) {
  let mut j = 0;
  while j <= i {
      if r.head != 0 {
          print!("*");
      } else {
          print!("0");
      }
      r = &r.next[0];
      j = j+1;
  }
  print!("\n");
}

fn compute_row(r: &mut L, j: i32) {
  let mut v = 0;
  if j == 0 {
    v = 1;
  } else {
    v = (get(r, j) + get(r, j-1)) % 7;
  }
  set(r, j, v);
  if j > 0 {
    compute_row(r, j-1);
  }
}

fn main() {
  let h = 42;
  let mut r = make(h+1);
  let mut i = 0;
  while i < h {
      set(&mut r, i, 0);
      compute_row(&mut r, i);
      print_row(&r, i);
      i = i+1;
  }
}
