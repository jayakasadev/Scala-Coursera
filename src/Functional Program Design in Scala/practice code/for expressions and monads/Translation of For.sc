/*
1. for (x1 <- x) yield y
  translated to
  x.map(x1 => y)

2. for(
    x <- e1 if filter; sequence of (empty or nonempty)generators and filters
  ) yields y

  translated to

  for (
    x <- e1.withFilter(x => f);
    sequence of (empty or nonempty)generators and filters
  ) yields y

  withFilter is a lazy version of filter because it does not create a new DS right
  away of all filtered elements because it is wasteful
  Instead, it remembers that any following call to map or flatMap has to be filtered
  by function f
  just a smarter version of filter

3. for(
      x <- e1;
      y <- e2;
      sequence of (empty or nonempty)generators and filters
    ) yields e3

    translates to

    e1.flatMap(x => for(
                          y <- e2;
                          sequence of (empty or nonempty)generators and filters
                        ) yields e3
    )

 */
val n = 10

val out = for {
  i <- 1 until n
  j <- 1 until i
  if i*j > i + j
} yield(i, j)

val out2 = (1 until n).flatMap(
  i => (1 until i).withFilter(j => (i + j < i * j)).map(j => (i, j))
)

println(out)
println(out2)

println(out == out2)


case class Book(title: String, authors:List[String])

val books = List(
  Book("Green Eggs and Ham", List("Dr. Suess")),
  Book("Green Ham", List("Dr. Dre", "OJ")),
  Book("Eggs and Ham", List("Dr. House")),
  Book("and Ham", List("House Suess", "TI")),
  Book("Green Eggs", List("McJagger", "OJ", "TI"))
)

val test1 = for(book <- books; a <- book.authors if a startsWith("Dr")) yield book.title

val test2 = books flatMap(
  book => book.authors.withFilter(author => author.startsWith("Dr")).map(author => book.title)
)

test1 == test2