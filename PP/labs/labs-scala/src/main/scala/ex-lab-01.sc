import scala.annotation.tailrec
import scala.math._


// 1.1.1 - factorialul unui numar
def fact (n: Int): Int = {

  @tailrec
  def aux_fact(n: Int, acc: Int): Int =
    if (n == 0) acc
    else aux_fact(n - 1, n * acc)

  aux_fact(n, 1)
}


// 1.1.2 - gratest common divisor


@tailrec
def gcd(a: Int, b: Int): Int = {
  if (a > b) gcd(a - b, b)
  else if (b > a) gcd (a, b - a)
  else a
}



// 1.1.3
def sumSquares(n: Int): Int = {

  @tailrec def helper(x: Int, acc: Int): Int = {
    if (x == 0) acc
    else helper(x - 1, acc + x * x)
  }

  helper(n, 0)
}



// 1.1.4

// direct recursion
def sumNats(start: Int, stop: Int): Int = {
  if (start == stop) start
  else if (start > stop) -1     // edge case
  else start + sumNats(start + 1, stop)
}



def tailSumNats(start: Int, stop: Int): Int = {

  @tailrec
  def helper(begin: Int, end: Int, acc: Int): Int = {
    if (begin > end) acc
    else helper(begin + 1, end, acc + begin)
  }

  if (start > stop) -1            // edge case
  else helper(start, stop, 0)
}



// 1.1.5
def sumPrimes(start: Int, stop: Int): Int = {

  @tailrec
  def isPrimeHelper(x: Int, div: Int): Boolean = {
    if (x == div) true
    else if (x % div == 0) false
    else isPrimeHelper(x, div + 1)
  }

  def isPrime(x: Int): Boolean = isPrimeHelper(x, 2)


  @tailrec
  def sumHelper(begin: Int, end: Int, acc: Int): Int = {
    if (begin > end) acc
    else if (isPrime(begin)) sumHelper(begin + 1, end, acc + begin)
    else sumHelper(begin + 1, end, acc)
  }

  def sum(): Int = sumHelper(start, stop, 0)

  sum()
}


// 1.1.6 - foldLeft
@tailrec
def substractRange(x: Int, start: Int, stop: Int): Int = {
  if (start > stop) throw new Exception("Invalid interval")
  else if (start == stop) x
  else substractRange(x - start, start + 1, stop)
}


// 1.1.7 - folRight, direct recursion
def foldRightSubstractRange(x: Int, x0: Int, xn: Int): Int = {
  if (x0 > xn) throw new Exception("Edge case: x0 > xn")
  else if (x0 == xn) xn - x
  else x0 - foldRightSubstractRange(x, x0 + 1, xn)
}




// 1.2.1
// x_n+1 = 1 / 2 * (x_n + a / x_n)
def improve(xn: Double, a: Double): Double = {
  if (xn == 0.0) throw new Exception("Illegal division by zero!!")
  else (xn + a / xn) / 2.0
}


// 1.2.2
def nth_guess(n: Int, a: Double): Double = {

  @tailrec
  def helper(N: Int, xn: Double): Double = {
    if (N == 0) xn
    else helper(N - 1, improve(xn, a))
  }

  helper(n, 1)    // x0 == 1
}



// 1.2.3
def acceptable(xn: Double, a: Double): Boolean = {
  // 0.001 in forma stiintifica: 1.e * e ^ -3
  abs(xn * xn - a) <= 1.0e-3
}



// 1.2.4
def mySqrt(a: Double): Double = {
  def improve(xn: Double): Double = (xn + a / xn) / 2.0

  def acceptable(xn: Double): Boolean =
    abs(xn * xn - a) <= 1.0e-3 || scala.math.abs((xn - improve(xn)) / xn) <= 1.0e-3


  // estimate == xn
  @tailrec
  def tailSqrt(estimate: Double): Double =
    if (acceptable(estimate) == true) estimate
    else tailSqrt(improve(estimate))

  tailSqrt(1.0)   // x0 == 1
}



/*     TESTING    */

// 1.1.1
println(fact(1))
println(fact(2))
println(fact(3))
println(fact(4))
println(fact(5))
println()


// 1.1.2
println(gcd(101, 13))
println(gcd(70, 25))
println(gcd(100, 500))
println(gcd(500, 125))
println(gcd(700, 895))
println()


// 1.1.3
println(sumSquares(1))
println(sumSquares(2))
println(sumSquares(3))
println(sumSquares(4))
println(sumSquares(5))
println()


// 1.1.4
// string interpolation:    s"(${a}, ${b})"
println(s"(${sumNats(1, 1)} == ${tailSumNats(1, 1)})")
println(s"(${sumNats(-1, 2)} == ${tailSumNats(-2, 2)})")
println(s"(${sumNats(1, 3)} == ${tailSumNats(1, 3)})")
println(s"(${sumNats(2, 5)} == ${tailSumNats(2, 5)})")
println(s"(${sumNats(5, 5)} == ${tailSumNats(5, 5)})")



// 1.1.5
println(s"Sum of Primes in [2, 2] = " + sumPrimes(2, 2))
println(s"Sum of Primes in [2, 5] = " + sumPrimes(2, 5))
println(s"Sum of Primes in [5, 10] = " + sumPrimes(5, 10))


// 1.1.6
println(substractRange(10, 1, 5))
println(substractRange(10, 2, 4))
println(substractRange(10, 3, 3))
try {
  println(substractRange(10, 4, 2))
} catch {
  case e: Exception => println(e.getMessage)
}

// 1.1.7
println(foldRightSubstractRange(10, 1, 5))
println(foldRightSubstractRange(10, 2, 4))
println(foldRightSubstractRange(10, 3, 3))
try {
  println(foldRightSubstractRange(10, 4, 2))
} catch {
  case e: Exception => println(e.getMessage)
}
println("good")


// 1.2.5
println(mySqrt(2.0e50))
println(mySqrt(2.0e-50))

