def data = List(1, 2, 3, 4, 5, 6)

def sum(xs: List[Int]): Int = xs match {
  case Nil => 0
  case y :: ys => y + sum(ys)
}

sum(data)

def sum1(xs: List[Int]) = (0 :: xs) reduceLeft ((x, y) => x + y)
sum1(data)

def sum2(xs: List[Int]) = (xs foldLeft 0) (_ * _)

sum2(data)

//abstract class List[T] {
//  def reduceLeft(op: (T, T) => T): T = this match {
//    case Nil => throw new Error("Nil.reduceLeft")
//    case x :: xs => (xs foldLeft x) (op)
//  }
//
//  def foldLeft[U](z: U)(op: (U, T) => U): U = this match {
//    case Nil => z
//    case x :: xs => (xs foldLeft op(z, x)) (op)
//  }
//}

def concat[T](xs: List[T], ys: List[T]) : List[T] = (xs foldRight ys)(_ :: _)

val list = List(1,2,3,4,5)
concat(list,list)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( ??? )

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( ??? )