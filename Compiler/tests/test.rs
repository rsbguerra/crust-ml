struct Point{
  x:i32,
  y:i32
}

fn test() ->i32{
  return 1;
}

fn main(){
  let mut a : i32 = 666;
  
  let mut b : i32 = 42 + a;

  //let a : &i32 = &b;

  println!(b);

  return;
}

