# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rails db:seed command (or created alongside the database with db:setup).
#
# Examples:
#
#   movies = Movie.create([{ name: 'Star Wars' }, { name: 'Lord of the Rings' }])
#   Character.create(name: 'Luke', movie: movies.first)

User.create!(name:  Faker::GameOfThrones.character,
             email: "example@railstutorial.org",
             password:              "foobarfoobar",
             password_confirmation: "foobarfoobar",
             admin: true,
             activated: true,
             activated_at: Time.zone.now)

99.times do |n|
    name  = Faker::GameOfThrones.character
    email = "example-#{n+1}@railstutorial.org"
    password = "passwordpassword"
    User.create!(name:  name,
                 email: email,
                 password:              password,
                 password_confirmation: password,
                 activated: true,
                 activated_at: Time.zone.now)
end

users = User.order(:created_at).take(6)
50.times do
  content = Faker::Hipster.sentence(5)
  users.each { |user| user.microposts.create!(content: content) }
end
