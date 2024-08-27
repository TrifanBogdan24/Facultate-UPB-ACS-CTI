import scala.annotation.tailrec

println("Hello world!")


def fact (n: Int): Int = {
  """
    | problema 1.1.1
    | returneaza factorialul unui numar
    |""".stripMargin
  @tailrec
  def aux_fact(n: Int, acc: Int): Int =
    if (n == 0) acc
    else aux_fact(n - 1, n * acc)
  aux_fact(n, 1)
}


println("5! = " + fact(5))
println("2! = " + fact(2))

@tailrec
def gcd(a: Int, b: Int): Int = {
  """
    | problema 1.1.2
    |""".stripMargin
  if (a < b) gcd(a, b - a)
  else if (a > b) gcd(a - b, b)
  else a
}


println("gcd(12, 49) = " + gcd(12, 49))
println("gcd(12, 24) = " + gcd(12, 24))
println("gcd(12, 39) = " + gcd(12, 39))



def sumSquares(n: Int): Int = {
  """
    | problema 1.1.3
    |""".stripMargin
  @tailrec
  def sumrec(n: Int, sum: Int): Int = {
    if (n == 0) sum
    else sumrec(n - 1, sum + n * n)
  }

  sumrec(n, 0)
}

println("sumSquares(1) = " + sumSquares(1)) // 1
println("sumSquares(2) = " + sumSquares(2)) // 1 + 4
println("sumSquares(3) = " + sumSquares(3)) // 1 + 4 + 9
println("sumSquares(4) = " + sumSquares(4)) // 1 + 4 + 9 + 16
println("sumSquares(5) = " + sumSquares(5)) // 1 + 4 + 9 + 16 + 25



def sumNats(start: Int, stop: Int): Int = {
  """
    | problema 1.1.4
    |""".stripMargin
  if (start > stop) 0
  else start + sumNats(start + 1, stop)
}


println("sum[-5:0]) = " + sumNats(-5, 0))
println("sum[0:5]) = " + sumNats(0, 5))
println("sum[-5:5]) = " + sumNats(-5, 5))



def tailSumNats(start: Int, stop: Int): Int = {
  """
    | problema 1.1.4 (varianta tail recursion)
    |""".stripMargin
  @tailrec
  def tailRecSum(start: Int, stop: Int, sum: Int): Int = {
    if (start > stop) sum
    else tailRecSum(start + 1, stop, sum + start)
  }

  tailRecSum(start, stop, 0)
}


println("sum[-5:0]) = " + tailSumNats(-5, 0))
println("sum[0:5]) = " + tailSumNats(0, 5))
println("sum[-5:5]) = " + tailSumNats(-5, 5))


def sumPrimes(start: Int, stop: Int): Int = {
  """
    | problema 1.1.5
    | calculeaza suma numerelelor prime dintr-un interval
    |""".stripMargin



  @tailrec
  def isPrimeHelper(nr: Int, div: Int): Boolean = {
    """
      | verifica daca un numar este prim folosind un acumulator
      | pe care il folosesc drept divizor
      |""".stripMargin
    if (div > nr / 2) true
    else if (nr % div == 0) false
    else isPrimeHelper(nr, div + 1)
  }


  def isPrime(nr: Int): Boolean = {
    """
      | true -> numarul este prim
      | false -> numarul nu este prim
      |""".stripMargin
    isPrimeHelper(nr, 2)
  }


  @tailrec
  def sumPrimeNumbers(start: Int, stop: Int, sum: Int): Int = {
    """
      | calculeaza suma efectiva a numerelor prime
      |""".stripMargin
    if (start > stop) sum
    else {
      if (isPrime(start) == true) sumPrimeNumbers(start, stop, sum + start)
      else sumPrimeNumbers(start, stop, stop)
    }
  }


  sumPrimeNumbers(start, stop, 0)
}





@tailrec
def substractRange(x: Int, start: Int, stop: Int): Int = {
  """
    | problema 1.1.6
    |""".stripMargin
  if (start > stop) x
  else substractRange(x - start, start + 1, stop)
}


println(substractRange(1, 1, 2))  // 1 - 1 - 2
println(substractRange(1, 1, 3))  // 1 - 1 - 2 - 3
println(substractRange(1, 1, 4))  // 1 - 1 - 2 - 3 - 4


def substractRengeWithSign(x: Int, start: Int, stop: Int): Int = {
  """
    | problema 1.1.7
    |""".stripMargin
  @tailrec
  def changeSubstractionSign(x: Int, sign: Int, start: Int, stop: Int): Int = {
    if (start > stop) x * sign
    else changeSubstractionSign(x - sign * start, -sign, start + 1, stop)
  }

  changeSubstractionSign(x, 1, start, stop)
}


println(substractRengeWithSign(1, 1, 2))  // 1 - 1 + 2
println(substractRengeWithSign(1, 1, 3)) // 1 - 1 + 2 - 3
println(substractRengeWithSign(1, 1, 4)) // 1 - 1 + 2 -3 + 4



@tailrec
def improve(xn: Double, a: Double): Double = {
  """
    | problema 1.2.1
    | calculeaza radicalul unuui numar
    |""".stripMargin
  if (xn == 1.0) xn
  else {
    val new_xn = (xn + a / xn) / 2.0
    improve(new_xn, a)
  }
}


println(improve(1.0, 49.0))
