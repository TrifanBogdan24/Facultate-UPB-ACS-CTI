import annotation.tailrec

// 2.1.1
def apply(n: Int, f: Int => Int): Int = f(n)

// 2.1.2
def doubler(): Int => Int = {
  (n: Int) => 2 * n   // expresie lambda (functie anonima)
}




/** 2.1.3
 *
 * @param t   `try` function
 * @param c   `catch` function
 * @param x
 * @return
 */
def trycatch(t: Int => Int, c: Int => Int)(x: Int): Int = {
  if (t(x) == 0) c(x)
  else t(x)
}


/** 2.1.4
 *
 * @param t   `try` function
 * @param c   `catch` function
 * @return
 *
 *
 * functia primestei doua argumente de tip call-by-name (`=>`)
 * si returneaza un `Int`.
 *
 * Call-by-name in Scala inseamna ca argumentele nu sunt evaluate atunci cand functia este apelata,
 * ci doar atunci cand sunt efectiv utilizate in corpul functiei
 */
def realtrycatch(t : => Int, c: => Int): Int  = {
  if (t == 0) t
  else c
}



// 2.2.1
def foldWith (op: (Int,Int) => Int)(start: Int, stop: Int): Int = {

  @tailrec
  def tail_fold(crt: Int, acc: Int): Int  = {
    if (crt > stop) acc
    tail_fold(crt + 1, op(acc, start))
  }

  tail_fold(start + 1, start)
}



// 2.2.2
def foldConditional(op: (Int,Int) => Int, p: Int => Boolean)(start: Int, stop: Int): Int = {

  @tailrec
  def get_first_to_pass_predicate(idx: Int): Int = {
    if (idx > stop) -1
    else if (p(idx) == true) idx
    else get_first_to_pass_predicate(idx + 1)
  }

  @tailrec
  def tail_fold(idx: Int, acc: Int): Int = {
    if (idx > stop) acc
    else if (p(idx) == true) tail_fold(idx + 1, op(acc, idx))
    else tail_fold(idx + 1, acc)
  }


  val idx: Int = get_first_to_pass_predicate(start)

  if (idx == -1) throw new Exception("No element satisfiest the condition")
  else tail_fold(idx + 1, idx)
}


// 2.2.3
def foldMap(op: (Int,Int) => Int, f: Int => Int)(start: Int, stop: Int): Int = {

  @tailrec
  def helper(idx: Int, acc: Int): Int = {
    if (idx > stop) acc
    else helper(idx + 1, op(acc, f(idx)))
  }

  if (start > stop) throw new Exception("Invalid interval")

  helper(start + 1, f(start))
}



// 2.3.1
def uncurryMultiply(x:Int, y:Int): Int = x * y
def curryMultiply(x:Int)(y:Int): Int = x * y

// 2.3.2
def uncurryCompare(x: Int, y: Int, z: Int): Int = {
  if x > y && x > z then
    x
  else if y > x && y > z then
    y
  else
    z
}



def curryCompare(x: Int)(y: Int)(z: Int): Int = {
  if (x > y && x > z) x
  else if (y > x && y > z) y
  else z
}




// 2.4 Funcion transformation
// f(x) = a * x + b

// 2.4.1 - shift OY
def shiftOY(line: Double => Double, delta_y: Double): Double => Double = {
  (x: Double) => line(x) + delta_y
}


// 2.4.2 - shift OX
def shiftOX(line: Double => Double, delta_x: Double): Double => Double = {
  (x: Double) => line(delta_x + x)
}


/**
 *
 * @param line1   ecuatia primei drepte             line1(x) = a1 * x + b1
 * @param line2   ecuatia celei de a doua drepte    line2(x) = a2 * x + b2
 * @param start
 * @param stop
 * @return
 *
 *
 *
 * x = ? astfel incat line1(x) = line2(x)
 *                  <=> a1 * x + b1 = a2 * x + b2
 *                  <=> (a1 - a2) * x = b2 - b1
 *                  <=> x = (b2 - b1) / (a1 - a2), daca a1 != a2
 *
 *                  (a1 == a2  caz in care liniile sunt paralele si nu se intersecteaza)
 */
// 2.4.3 - intersection <=> exista x = ? a.i line1(x) == line2(x)
def intersect(line1: Double => Double, line2: Double => Double)(start: Int, stop: Int): Boolean = {

  def get_line_coeff(line: Double => Double): (Double, Double) = {
    val a = (line(1) - line(0))     // panta dreptei : a == f(1) - f(0) = (a + b) - b
    val b = line(0)                 // termenul liber: b == f(0) = a * 0 + b = b
    (a, b)                          // f(x) = a * x + b
  }

  // extragem coeficientii dreptelor in cate in tuplu
  val (a1, b1) = get_line_coeff(line1)
  val (a2, b2) = get_line_coeff(line2)


  if (a1 == a2) {
    println("Liniile sunt paralele: nu se pot intersecta")
    false
  } else {

    val x = (b2 - b1) / (a1 - a2)

    // expresia de mai jos intoarce un Double
    start.toDouble <= x && x <= stop.toDouble
  }
}


/*       TESTING    */
println(curryCompare(1)(2)(3))
println(curryCompare(2)(3)(1))
println(curryCompare(3)(1)(2))
