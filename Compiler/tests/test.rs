fn test(a:&mut i32)
{
   *a = 3;
    return;
}

fn main()
{  
  let mut x:i32 = 52;
  
  test(&mut x);

  println!(x);
  
  return;
}
