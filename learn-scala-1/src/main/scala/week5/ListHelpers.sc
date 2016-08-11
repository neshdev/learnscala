import scala.collection.immutable.::

def last[T](xs: List[T]) : T = xs match  {
  case List() => throw new Error("last of empty List")
  case List(x) => x
  case y :: ys => last(ys)
}

def init[T](xs: List[T]) : List[T] = xs match {
  case List() => throw new Error("init of empty List")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

def concat[T](xs: List[T], ys: List[T]) : List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]) : List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ++ List(y)
}

def take[T](n : Int, xs:List[T]) : List[T] = ???

def removeAt[T]( n: Int, xs: List[T]) : List[T] = (xs take(n)) ::: (xs drop(n+1))

def flatten(xs: List[Any]) : List[Any] = xs match {
  case Nil => Nil
  case (y :: ys) :: yss => flatten(y :: ys) ++ flatten(yss)
  case y :: ys => y :: flatten(ys)
}


val list = List(1,2,3,4,5)
last(list)
init(list)
concat(list,list)
reverse(list)
removeAt(1,list)

flatten(List(List(1, 1), 2))
flatten(List(List(1, 1), 2, List(3, List(5, 8))))