package week2.aHigherOrderFunctions

object HigherOrderFunctions {
  def mapReduce(f: Int => Int, combineF: (Int, Int) => Int, unitValue :Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a+1, combineF(acc, f(a)))
    }
    loop(a, unitValue)
  }

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    mapReduce(f, (x,y)=>x+y, 0)(a,b)
  }

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    mapReduce(f, (x,y)=>x*y, 1)(a,b)
  }

  def factorial(n: Int): Int = {
    product(x => x)(1, n)
  }

  def sum_v1(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a+1, acc+f(a))
    }
    loop(a, 0)
  }

  def product_v1(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a+1, acc*f(a))
    }
    loop(a, 1)
  }
}
