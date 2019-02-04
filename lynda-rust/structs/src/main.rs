fn main() {
	struct User {
		username: String,
		email: String,
		sign_in_count: u64,
		active: bool,
	};

	let user1 = User {
		email: String::from("name@example.com"),
		username: String::from("Kat"),
		active: true,
		sign_in_count: 1,
	};

	println!("{}", user1.email);

	let user2 = User {
		email: String::from("user2@example.com"),
		username: String::from("tak"),
		..user1
	}
}

fn build_user (email: String, username: String) -> User {
	User{
		email,
		username,
		active: true,
		sign_in_count: 1,
	}
}