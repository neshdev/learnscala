val  n = 7
(1 until n).map(i =>
  (1 until  i) map ( j => (i, j))
).flatten


def isPrime(n:Int) : Boolean = {
  (2 until n).forall(d => n % d != 0)
}

(1 until n).flatMap(i =>
  (1 until  i) map ( j => (i, j))
) filter ( pair => isPrime(pair._1 + pair._2))

//case class Person(name: String, age: Int)
//val persons : List[Person]
//for (p <- persons if p.age > 20) yield  p.name

//much more simple version than above using maps filters and flat maps
for {
  i <- 1 until n
  j <- 1 until i
  if (isPrime(i+j))
} yield (i, j)

def scalarProduct(xs: List[Double], ys: List[Double]) : Double =
(for (
  (x,y) <- xs zip ys
) yield x*y).sum


val ys1 = List[Double](5,6,7,8)
val xs1 = List[Double](1,2,3, 4)


scalarProduct(xs1,ys1)