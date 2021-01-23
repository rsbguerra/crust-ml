
fn print_nat(n: i32) {
    if n > 9 { print_nat(n / 10); }
    let d = n % 10;
    if d == 0 { print!("0"); }
    if d == 1 { print!("1"); }
    if d == 2 { print!("2"); }
    if d == 3 { print!("3"); }
    if d == 4 { print!("4"); }
    if d == 5 { print!("5"); }
    if d == 6 { print!("6"); }
    if d == 7 { print!("7"); }
    if d == 8 { print!("8"); }
    if d == 9 { print!("9"); }
}

fn print_int(n: i32) {
    if n == -2147483648 { print!("-2147483648"); return; }
    if n < 0 { print!("-"); print_nat(- n); }
    else { print_nat(n); }
}

fn main() {
  print_int(0); print!("\n");
  print_int(-42); print!("\n");
  print_int(42); print!("\n");
  print_int(769678); print!("\n");
  print_int(2147483647); print!("\n");
  print_int(-2147483648); print!("\n");
}
