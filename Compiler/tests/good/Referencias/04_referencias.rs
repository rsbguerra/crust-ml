fn main()
{
  let mut x : i32 = 4;
  let x : &mut i32 = &mut x;
  
  println!(*x);

  return;
}
