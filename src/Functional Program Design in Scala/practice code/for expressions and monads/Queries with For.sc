//case class Author(name: String)
case class Book(title:String, authours: List[String])

val books: List[Book] = List(
  Book(title = "Green Eggs and Ham", authours = List("Dr. Suess")),
  Book(title = "Apple", authours = List("Dr. Who", "MJ", "Yeezus")),
  Book(title = "Ham", authours = List("Dr. House", "MJ")),
  Book(title = "Green Eggs", authours = List("BowWow", "TI", "TPain"))
)

//distinct authors
val authors = {
  for {
    book1 <- books
    book2 <- books
    if (book1.title != book2.title)
    author1 <- book1.authours
    author2 <- book2.authours
    if(author1 != author2)
  } yield author1
}.distinct
println(authors)

//common authors
val authors2 = {
  for {
    book1 <- books
    book2 <- books
    if (book1.title != book2.title)
    author1 <- book1.authours
    author2 <- book2.authours
    if(author1 == author2)
  } yield author1
}.distinct
println(authors2)