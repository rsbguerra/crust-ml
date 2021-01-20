struct Boolean{
    v: i32
}

struct Point2D{
    x: i32,
    y: i32
}

fn main(){
    let test : Point2D = Boolean{v:true};

    println!(test.x);

    return;
}
