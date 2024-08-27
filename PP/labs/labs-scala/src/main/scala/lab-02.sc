
import scala.annotation.tailrec



def apply(n: Int, f: Int => Int): Int = {
  """
    | 2.1.1
    |""".stripMargin
  f(n)
}


print(apply(5, 2 * _))
print(apply(5, 4 - 3 * _))
print(apply(5, 3 - _))
print(apply(5, 2 + _))


def doubler(): Int => Int = {
  """
    | 2.1.2
    |""".stripMargin
  (x: Int) => x * 2
}



print(apply(5, doubler()))
print(apply(10, doubler()))
print(apply(15, doubler()))


def trycatch(t: Int => Int, c: Int => Int)(x: Int): Int = {
  """
    | 2.1.3
    | t = tray (functie)
    | c = catch (functie)
    |
    | va returna valoarea functiei in x
    |""".stripMargin

  if (t(x) == 0) c(x)
  else t(x)
}


def realtrycatch(t : => Int, c: => Int): Int  = {
  """
    | 2.1.4
    | t = try (functie)
    | c = catch (functie)
    |
    | va returna o functie
    |""".stripMargin


  if (t == 0) c
  else t
}


realtrycatch({
  val x = 10
  val y = x - 5
  y
  // ramane in try
}, {
  println("is in catch")
  1
})

realtrycatch({
  0
}, {
  // va intra aici
  println("is in catch")
  1
})



def foldWith(op: (Int, Int) => Int)(start: Int, stop: Int): Int = {
  """
    | 2.2.1
    |""".stripMargin

  def tail_fold(crt: Int, acc: Int): Int =
    if (crt > stop) acc
    else tail_fold(crt + 1, op(acc, crt))

  tail_fold(start, 0)
}

def foldConditional(op: (Int, Int) => Int, p: Int => Boolean)(start: Int, stop: Int): Int = {
  """
    | 2.2.2
    |""".stripMargin
  def tail_fold(crt: Int, acc: Int): Int =
    if (crt > stop) acc
    else if (p(crt)) tail_fold(crt + 1, op(acc, crt))
    else tail_fold(crt + 1, acc)

  tail_fold(start, 0)
}

println("here 1")


def foldMap(op: (Int, Int) => Int, f: Int => Int)(start: Int, stop: Int): Int = {
  """
    | 2.2.3
    |""".stripMargin
  foldWith((acc, crt) => op(acc, apply(crt, f)))(start, stop)
}


println(foldWith((x, y) => x + y)(1, 3)) // correct 6

val isEven: Int => Boolean = _ % 2 == 0
println(foldConditional(_ + _, isEven)(1, 5)) // correct: 6 (1 + 3 + 5)

val square: Int => Int = x => x * x
println(foldMap(_ + _, square)(1, 3)) // correct: 14 (1*1 + 2*2 + 3*3 = 1 + 4 + 9 = 14)
println("here 2")


def uncurry_multiply(x: Int, y: Int): Int = {
  x + y
}


println(uncurry_multiply(4, 8))

println("here 3")

def curry_multiply(x: Int)(y: Int): Int = x * y

println(curry_multiply(4)(8))


def un_curry_compare(x: Int, y: Int, z: Int): Int =
{
  if (x > y && x > z) x
  else if (y > x && y > z) y
  else z
}




def curry_compare(x: Int)(y: Int)(z: Int): Int = {
  if (x > y && x > z) x
  else if (y > x && y > z) y
  else z
}


val lambda_func = curry_compare(1)_
val lambda_func2 = lambda_func(2)
val ret_val = lambda_func2(3)
println(ret_val)

print(curry_compare(1)(2)(3))
print(curry_compare(2)(1)(3))
print(curry_compare(3)(1)(2))
print(curry_compare(2)(1)(3))


def shiftOY(line: Double => Double, delta_y: Double): Double => Double = {
  """
    | 2.4.1 - modificam termenul liber
    | a * x + (b + delta)
    |""".stripMargin
  (x: Double) => line(x) + delta_y
}

def dreapta(x: Double): Double = 2 * x + 3

val shiftedLine = shiftOY(dreapta, 5)
println(shiftedLine(2))
// dreapta(2) + delta_y = 2*2 + 3 + 5 = 13)


def shiftOX(line: Double => Double, delta_x: Double): Double => Double = {
  """
    | 2.4.2 - modificam coeficientul lui x
    | (a - delta) * x + b
    |""".stripMargin
  (x: Double) => line(x + delta_x)
}


//def dreapta(x: Double): Double = 2 * x + 3
//
//val shiftedLine = shiftOX(dreapta, 2)
//println(shiftedLine(5))
//// O(dreapta(5 - delta_x) = dreapta(3) = 2*3 + 3 = 9 + 3 = 13


println("here")

def intersect(line1: Double => Double, line2: Double => Double)(start: Int, stop: Int): Boolean = {
  """
    | 2.4.3
    |""".stripMargin
  @tailrec
  def intersectHelper(current: Int): Boolean = {
    if (current > stop) false
    else if (line1(current) == line2(current)) true
    else intersectHelper(current + 1)
  }

  intersectHelper(start)
}


def line1(x: Double): Double = 2 * x + 3
def line2(x: Double): Double = -3 * x + 10

val intersection = intersect(line1, line2)(1, 5)
println(intersection)


println("reached all instructions")
