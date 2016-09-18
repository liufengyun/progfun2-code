object forexprs {
  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    for (x <- xs) yield f(x)

  def flatMap[T, U](xs: List[T], f: T => Iterable[U]): List[U] =
    for (x <- xs; y <- f(x)) yield y

  def filter[T](xs: List[T], p: T => Boolean): List[T] =
    for (x <- xs if p(x)) yield x


  case class Book(title: String, authors: List[String])

  val books = Set(
    Book(
      title = "Structure and Interpretation of Computer Programs",
      authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(
      title = "Introduction to Functional Programming",
      authors = List("Bird, Richard", "Wadler, Phil")),
    Book(
      title = "Effective Java",
      authors = List("Bloch, Joshua")),
    Book(
      title = "Effective Java 2",
      authors = List("Bloch, Joshua")),
    Book(
      title = "Java Puzzlers",
      authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(
      title = "Programming in Scala",
      authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))

  // Original
  for (b <- books; a <- b.authors if a startsWith "Bird") yield b.title


  // First substitution
  books.flatMap(b => for (a <- b.authors if a startsWith "Bird") yield b.title)

  // Second substitution
  books.flatMap(b => for (a <- b.authors.withFilter(a => a startsWith "Bird")) yield b.title)

  // Third substitution
  books.flatMap(b => b.authors.withFilter(a => a startsWith "Bird").map(_ => b.title))
}