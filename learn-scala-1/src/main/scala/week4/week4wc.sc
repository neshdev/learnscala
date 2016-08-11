import week4._

object App {
  def nth[T](n: Int, list: List[T]): T = {
    if (list.isEmpty) throw new IndexOutOfBoundsException("list.isEmpty")
    if (n == 0) list.head
    else nth(n - 1, list.tail)
  }

  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
  nth(2, list)
  nth(-1, list)
  nth(4, list)
}