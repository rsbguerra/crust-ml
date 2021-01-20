fn ToN(i : i32, n : i32)
{
    if (i > n)
    {
        return;
    }
    
    println!(i);
    
    return ToN(i+1, n);
}

fn main()
{
  ToN(0, 9);
  
  return;
}
