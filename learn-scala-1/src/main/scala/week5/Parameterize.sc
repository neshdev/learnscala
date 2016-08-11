def msort[T](xs: List[T])(lt: (T,T) => Boolean) : List[T] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    def merge(xs:List[T], ys:List[T]) : List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y::ys1) =>
        if ( lt(x,y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }
    val (fst, snd) = xs splitAt n
    merge(msort(fst)(lt), msort(snd)(lt))
  }
}
val  nums = List(10,9,8,7,6,5,4)
val fruits = List("apple", "pineapple", "orange", "banana")
msort(nums)((x: Int, y: Int ) => x < y)
msort(fruits)((x,y)=> x.compareTo(y) < 0)