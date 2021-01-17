fn main()
{
  let mut x : i32 = 4;
  let x : &i32 = &mut x;
  
  println!(*x);

  return;
}
