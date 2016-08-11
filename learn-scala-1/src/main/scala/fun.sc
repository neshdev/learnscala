def fib(n: Int): Int = {
  if (n == 0) 0
  else if (n == 2 || n == 1) 1
  else fib(n - 1) + fib(n - 2)
}

fib(2)
fib(3)
fib(4)
fib(5)
fib(6)

def fib_tail(n: Int): Int = {
  def loop(n: Int, a: Int, b: Int): Int = {
    if (n == 0) b
    else loop(n - 1, b, a + b)
  }
  if (n <= 0) 0
  else loop(n, 0, 1)
}

fib_tail(0)
fib_tail(1)
fib_tail(2)
fib_tail(3)
fib_tail(4)
fib_tail(5)
fib_tail(6)

//fib_tail(0) == 0
//fib_tail(1) == 1
//fib_tail(2) == 1
//fib_tail(3) == 2
//fib_tail(4)
//fib_tail(5) == 5

//
//def sum(f: Int => Int)(a:Int, b:Int): Int ={
//  def loop(a: Int, acc: Int): Int ={
//    if ( a > b) acc
//    else loop(a+1, f(a) + acc)
//  }
//  loop(a, 0)
//}
//
//sum(x=> x)(1,10)
//sum(x=> x * x)(1,100000)
//
//def fact(n: Int): Int ={
//  def loop(n: Int, acc: Int): Int ={
//    if ( n == 0) acc
//    else loop(n - 1, acc * n)
//  }
//  loop(n, 1)
//}
//
//fact(15)
//sum(fact)(1, 10)
//
//def product(f: Int => Int)(a: Int, b: Int) : Int = {
//  if (a > b) 1
//  else f(a) * product(f)(a + 1, b)
//}
//
//def fact1(x: Int) : Int = product(x=> x)(1, x)
//
//fact1(15)
//
//def mapReduce(f: Int => Int, combine: (Int,Int) => Int, zero: Int)(a: Int, b: Int) : Int = {
//  if (a > b) zero
//  else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
//}
//
//def mr_tail(f: Int => Int, combine: (Int,Int) => Int, zero: Int)(a: Int, b: Int) : Int = {
//  def loop(a: Int, acc: Int) : Int = {
//    if ( a > b) acc
//    else loop(a+1, combine(acc, f(a)))
//  }
//  loop(a, zero)
//}
//
//
//
//
//
//def sum2(a: Int, b: Int) : Int = mapReduce(x=> x, (x,y)=> x + y, 0)(a, b)
//def sum3(a: Int, b: Int) : Int = mr_tail(x=> x, (x,y)=> x + y, 0)(a, b)
//def product2(a:Int, b: Int) : Int = mapReduce(x=>x, (x,y)=> x * y, 1)(a, b)
//def product3(a:Int, b: Int) : Int = mr_tail(x=>x, (x,y)=> x * y, 1)(a, b)
//def fact2(x: Int) : Int = product2(1, x)
//def fact3(x: Int) : Int = product3(1, x)
//
//sum2(1,3)
//sum3(1,3)
//
//sum2(1,5000)
//sum3(1,5000)
//
//product2(1,3)
//product3(1,3)
//
//product2(1,15)
//product3(1,15)
//
//fact2(15)
//fact3(15)
//
