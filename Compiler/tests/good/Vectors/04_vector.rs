fn print_vec(v:Vec<i32>){
    let size:i32 = v.len();
    let mut i:i32 = 0;
    
    while(i<size){
        println!(v[i]);
        i = i+1;
    }
}

fn main() {
    let mut v: Vec<Vec<i32>> = vec![vec![1,2,3], vec![4,5,6], vec![7,8,9]];
    print_vec(v[0]);
    print_vec(v[1]);
    print_vec(v[2]);

    return;
}