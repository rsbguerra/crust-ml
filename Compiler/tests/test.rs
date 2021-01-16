struct Point{
  x:i32,
  y:i32
}

fn test(a:i32, b:i32) ->i32{
  return 1;
}

fn main(){
  let mut a : i32 = 666;
  let mut b : i32 = 42;
  
  
  //let a : &i32 = &b;

  println!(test(a, b));
  //println!(*a);

  return;
}

