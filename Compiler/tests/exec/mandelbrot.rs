
/* arithmetique de virgule fixe
   precision q = 8192 i.e. 13 bits pour la partie decimale */

fn add(x: i32, y: i32) -> i32 {
  x + y
}
fn sub(x: i32, y: i32) -> i32  {
  x - y
}
fn mul(x: i32, y: i32) -> i32  {
  let t = x * y;
  (t + 8192 / 2) / 8192
}
fn div(x: i32, y: i32) -> i32  {
  let t = x * 8192;
  (t + y / 2) / y
}
fn of_int(x: i32)  -> i32 {
  x * 8192
}

fn iter(n: i32, a: i32, b: i32, xn: i32, yn: i32) -> bool {
  if n == 100 { return true; }
  let xn2 = mul(xn, xn);
  let yn2 = mul(yn, yn);
  if add(xn2, yn2) > of_int(4) { return false; }
  iter(n+1, a, b, add(sub(xn2, yn2), a),
       add(mul(of_int(2), mul(xn, yn)), b))
}

fn inside(x: i32, y: i32) -> bool {
  iter(0, x, y, of_int(0), of_int(0))
}

fn run(steps: i32) {
  let xmin = of_int(-2);
  let xmax = of_int(1);
  let deltax = div(sub(xmax, xmin), of_int(2 * steps));
  let ymin = of_int(-1);
  let ymax = of_int(1);
  let deltay = div(sub(ymax, ymin), of_int(steps));
  let mut i = 0;
  while i < steps {
    let y = add(ymin, mul(of_int(i), deltay));
    let mut j = 0;
    while j < 2 * steps {
      let x = add(xmin, mul(of_int(j), deltax));
      if inside(x, y) {
        print!("0");
      } else {
        print!("1");
      }
      j = j+1;
    }
    print!("\n");
    i = i+1;
  }
}

fn main() {
  run(30);
}
