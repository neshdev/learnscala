import com.sun.org.apache.xalan.internal.utils.XMLSecurityManager.Limit

def scaleList(xs: List[Double], factor: Double): List[Double] = xs match {
  case Nil => xs
  case y :: ys => y * factor :: scaleList(ys, factor)
}

//abstract class List[T] {
//  def map[U](f: T=> U): List[U] = this match  {
//    case Nil => this
//    case x::xs => f(x) :: xs.map(f)
//  }
//}


def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => y * y :: squareList(ys)
}

var list = List(10d, 20d, 30d, 40d)
scaleList(list, 10d)

var list1 = List(10, 20, 30, 40)
squareList(list1)

def squareList1(xs: List[Int]): List[Int] = xs map (x => x * x)
squareList1(list1)

def posElems(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => if (y > 0) y :: posElems(ys) else posElems(ys)
}

val list3 = List(-1, 0, 534, -23, 55)
posElems(list3)

//abstract class List[T] {
//def filter (p: T => Boolean) : List[T] = this match  {
//  case Nil => this
//  case x :: xs => if (p(x)) x :: xs.filter(p) else xs.filter(p)
//}
//}

def posElems2(xs: List[Int]): List[Int] = xs filter (x => x > 0)
posElems2(list3)


val nums = List(2, -4, 5, 7, 1)
val fruits = List("apple", "pineapple", "orange", "banana")

nums filter (x => x > 0)
nums filterNot(x => x > 0)
nums partition( x=> x > 0)

nums takeWhile(x => x > 0)
nums dropWhile(x => x > 0)

nums span(x=> x > 0)


def pack[T](xs: List[T]) : List[List[T]] = xs match  {
  case Nil => Nil
  case x :: xs1 => {
    val (first, rest) = xs span (y => y == x)
    first :: pack(rest)
  }
}

val data = List("a", "a", "a", "b", "c", "c", "a")
pack(data)

def encode[T](xs : List[T]) : List[(T,Int)] = {
  val sublist = pack(xs)
  sublist.map(x => (x.head, x.length))
}

encode(data)




