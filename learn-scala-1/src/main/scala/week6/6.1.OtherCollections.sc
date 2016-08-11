val xs = Array(1,2,3, 4)


xs map (x => x* 2)

val s = "Hello World"
s filter (x => x.isUpper)

val r: Range = 1 until 5
val a: Range = 1 to 5

s exists(x => x.isUpper)
s.forall(x => x.isUpper)

val pairs = List(1,2,3) zip s
pairs unzip

s flatMap( x=> List('.',x))

xs.sum

xs.max


val N =10
val M = 5


(1 to M) flatMap (x=> (1 to N) map (y => (x,y)) )


def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map(xy => xy._1 * xy._2 ).sum

val ys1 = Vector[Double](5,6,7,8)
val xs1 = Vector[Double](1,2,3, 4)

(ys1 zip xs1)

scalarProduct(xs1,ys1)

def isPrime(n:Int) : Boolean = {
  (2 until N).forall(d => n % d != 0)
}

isPrime(19)