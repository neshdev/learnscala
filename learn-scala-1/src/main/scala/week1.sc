object excercise1 {
  def factorial(n: Int) : Int = {
    if ( n == 0) 1
    else n * factorial(n - 1)
  }

  val t0 = System.nanoTime()
  factorial(1000)
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + " ns")

  def factorial2(n: Int) : Int = {
    def loop(n: Int, acc: Int) : Int = {
      if ( n == 1) acc
      else loop(n-1, acc * n)
    }
    loop(n, 1)
  }

  val t3 = System.nanoTime()
  factorial2(1000)
  val t4 = System.nanoTime()
  println("Elapsed time: " + (t4 - t3) + " ns")
}