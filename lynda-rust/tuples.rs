fn main() {
	let my_tup: (i32, f64, u8) = (500, 3.5, 1);

	let (x, y, z) = my_tup; //destructuring

    let a = my_tup.0;
    let b = my_tup.1;

    println!("a = {}, b = {}", a,b);

	println!("x = {}, y = {}, z = {}", x,y,z);
}