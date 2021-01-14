/*struct Point2D
{
    x : i32, // 8
    y : i32  // 16
}

fn main() -> i32
{
    let p : Point2D = Point2D{x:6, y:7};
    
    print!(p.x); // p.x = p 
    // (pos at rbp) + Point2D[x] 
    // -(8 + 0) = -8
    
    return 0;
} 

*/

fn addOne(x:i32) ->i32
{
   if x > 10 {
       return x;
    }
    
    println!(x);

    return addOne(x+1);
}

fn main() -> i32
{
    println!(addOne(5));
 
    return 0;
} 
