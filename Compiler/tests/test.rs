struct Point{
  x:i32,
  y:i32
}

fn main(){
  let mut b : i32 = 42;
  let mut p : Point = Point{x:4, y:12};
  let mut p : Vec<i32> = Point{x:4, y:12};

  
  //let a : &i32 = &b;

  println!(p.y);
  //println!(*a);

  return;
}

