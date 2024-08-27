import annotation.tailrec

// 4.1 Natural Numbers

trait Nat
case object Zero extends Nat
case class Succ(x: Nat) extends Nat



// 4.1.3
def toNat(x: Int): Nat =
  if (x == 0) Zero
  else Succ(toNat(x - 1))

// 4.1.3
def NatToInt(n: Nat): Int = {
  @tailrec def helper(m: Nat, x: Int): Int =
    if (m == n) x
    else helper(Succ(m), x + 1)

  helper(Zero, 0)
}



// 4.1.1
// converting Nat -> Int -> Nat
def add_v1(x: Nat, y: Nat): Nat = {
  val a: Int = NatToInt(x)
  val b: Int = NatToInt(y)
  toNat(a + b)
}


// 4.1.1
// `Succ` operations
def add_v2(x: Nat, y: Nat): Nat = {
  @tailrec def helper(a: Nat, b: Nat, acc: Nat): Nat = {
    if (a == x && b == y) acc
    else if (a != x && b == y) helper(Succ(a), b, Succ(acc))
    else if (a == x && b != y) helper(a, Succ(b), Succ(acc))
    else /* if (a != x && b != y) */ helper(Succ(a), Succ(b), Succ(Succ(acc)))
  }

  helper(Zero, Zero, Zero)
}


// 4.1.2
// converting Nat -> Int -> Nat
def multiply_v1(x: Nat, y: Nat): Nat = {
  val a: Int = NatToInt(x)
  val b: Int = NatToInt(y)
  toNat(a * b)
}


// 4.1.2
// adding `x` times `y`
def multiply_v2(x: Nat, y: Nat): Nat = {
  @tailrec def helper(a: Nat, acc: Nat): Nat = {
    if (a == x) acc
    else helper(Succ(a), add_v1(acc, y))
  }

  helper(Zero, Zero)
}



// 4.1.2
// adding `y` times `x`
def multiply_v3(x: Nat, y: Nat): Nat = {
  @tailrec def helper(a: Nat, acc: Nat): Nat = {
    if (a == y) acc
    else helper(Succ(a), add_v1(acc, x))
  }

  helper(Zero, Zero)
}

// 4.1.2
// Succ operations
def multiply_v4(x: Nat, y: Nat): Nat = {
  @tailrec def helper(a: Nat, b: Nat, acc: Nat): Nat = {
    if (a == x) acc
    else if (a != x && b == y) helper(Succ(a), Zero, acc)
    else /* if (a != x && b != y) */ helper(a, Succ(b), Succ((acc)))
  }

  helper(Zero, Zero, Zero)
}



// 4.2 Option


// 4.2.1
def realrealtrycatch(t: => Option[Int], c: => Int): Int = {
  t match
    case Some(func) => func
    case None => c
}


// 4.2.2
def toNatOpt(x: Int): Option[Nat] = {
  if (x < 0) None
  else Some(toNat(x))
}


// 4.2.3
def addOpt(x: Option[Nat], y: Option[Nat]): Option[Nat] = {
  (x, y) match
    case (Some(nr1), Some(nr2)) => Some(add_v1(nr1, nr2))
    case (_, _) => None
}



// 4.3 Binary Trees
trait BTree
case object EmptyTree extends BTree
case class Node(value: Int, left: BTree, right: BTree) extends BTree


/**
 * adauga o valoare in `ARBORELE BINAR DE CAUTARE`
 * daca valoarea exista deja, nu va adauga nimic
 * */
def addValInTree(tree: BTree, x: Int): BTree = {

  tree match {
    case EmptyTree => Node(x, EmptyTree, EmptyTree)
    case Node(value, leftTree, rightTree) => {

      if (x < value)
        Node(value, addValInTree(leftTree, x), rightTree)
      else if (x > value)
        Node(value, leftTree, addValInTree(rightTree, x))
      else tree
    }
  }

}


/**
 * intoarce `true` daca elementul se afla in arborele binar
 * intoarce `false` daca elementul nu se afla in arborele binar
 */
def isInTree(tree: BTree, x: Int): Boolean = {
  tree match {
    case EmptyTree => false
    case Node(value, leftTree, rightTree) => {

      val isInLeft = isInTree(leftTree, x)
      val isInRight = isInTree(rightTree, x)

      if (value == x) true
      else isInTree(leftTree, x) || isInTree(rightTree, x)
    }
  }
}


// 4.3.1
def depth(tree: BTree): Int = {
  tree match {
    case EmptyTree => 0
    case Node(_, left, right) => {
      val leftDepth: Int = depth(left)
      val rightDepth: Int = depth(right)

      if (leftDepth < rightDepth) 1 + rightDepth
      else 1 + leftDepth
    }
  }
}


// 4.3.2 = number of nodes in subtree
def subtree(tree: BTree): Int = {
  tree match {
    case EmptyTree => 0
    case Node(_, left, right) => {
      val nodesLeft = subtree(left)
      val nodesRight = subtree(right)

      (nodesRight + nodesLeft + 1)
    }
  }
}

// 4.3.3 = nr nodes with even `(_ % 2 == 0)` nr of children
def evenChildCount(tree: BTree): Int = {
  tree match
    case EmptyTree => 0
    case Node(_, left, right) => {
      val nrNodes: Int = subtree(tree) - 1

      if (nrNodes % 2 == 0) 1 + evenChildCount(left) + evenChildCount(right)
      else evenChildCount(left) + evenChildCount(right)
    }
}



// 4.3.4 -> flatten
// preordine: Radacina - Stanga - Dreapta
def flatten_preordine(tree: BTree): List[Int] = {
  tree match {
    case EmptyTree => Nil // List.empty[Int]
    case Node(value, left, right) => {
      /** opeatorii `:::` si `++` concateneaza doua liste **/
      List(value) ::: flatten_preordine(left) ::: flatten_preordine(right)
    }
  }
}

// 4.3.4 -> flatten
// inordine: Stanga - Radacina - Dreapta
def flatten_inordine(tree: BTree): List[Int] = {
  tree match {
    case EmptyTree => Nil // List.empty[Int]
    case Node(value, left, right) => {
      /** opeatorii `:::` si `++` concateneaza doua liste **/
      flatten_inordine(left) ::: List(value) ::: flatten_inordine(right)
    }
  }
}


// 4.3.4 -> flatten
// poastordine: Stanga - Dreapta - Radacina
def flatten_postordine(tree: BTree): List[Int] = {
  tree match {
    case EmptyTree => Nil // List.empty[Int]
    case Node(value, left, right) => {
      /** opeatorii `:::` si `++` concateneaza doua liste **/
      flatten_postordine(left) ::: flatten_postordine(right) ::: List(value)
    }
  }
}



// 4.3.5
def countNodes(tree: BTree, cond: Int => Boolean): Int = {
  tree match {
    case EmptyTree => 0
    case Node(value, left, right) => {
      if (cond(value) == true) 1 + countNodes(left, cond) + countNodes(right, cond)
      else countNodes(left, cond) + countNodes(right, cond)
    }
  }
}


// 4.3.6
def mirror(tree: BTree): BTree = {
  tree match
    case EmptyTree => EmptyTree
    case Node(value, left, right) => Node(value, right, left)
}


// 4.3.7
def append(tree1: BTree, tree2: BTree): Option[BTree] = {

  def helper(idx: Int, l: List[Int], tree: BTree): Option[BTree] = {
    if (idx >= l.length) Some(tree)
    else {
      if (!isInTree(tree, l(idx))) None
      else helper(idx + 1, l, addValInTree(tree, l(idx)))
    }
  }

  /** opeatorii `:::` si `++` concateneaza doua liste **/
  helper(0, flatten_preordine(tree1) ::: flatten_preordine(tree2), EmptyTree)
}


// 4.4 Expression evaluation
trait Expr
case class Atom(a: Int) extends Expr
case class Add(e1: Expr, e2: Expr) extends Expr
case class Mult(e1: Expr, e2: Expr) extends Expr


// 4.4.1
def evaluate(e: Expr): Int = e match {
  case Atom(a) => a
  case Add(e1, e2) => evaluate(e1) + evaluate(e2)
  case Mult(e1, e2) => evaluate(e1) * evaluate(e2)
}

// 4.4.2
// a * (b + c) -> a * b + a * c
def simplify(e: Expr): Expr = e match {
  case Atom(a) => Atom(a)
  case Add(e1, e2) => Add(e1, e2)

  case Mult(e1, e2) => {
    (e1, e2) match {
      // s1 * (s2 + s3)  --> s1 * s2 + s1 * s3
      case (Atom(sub1), Add(sub2, sub3)) =>
        Add(Mult(Atom(sub1), simplify(sub2)), Mult(Atom(sub1), simplify(sub3)))

      // (s1 + s2) * s3 --> s3 * s1 + s3 * s2
      case (Add(sub1, sub2), Atom(sub3)) =>
        Add(
          Mult(Atom(sub3), simplify(sub1)),   // s3 * s1
          Mult(Atom(sub3), simplify(sub2))    // s3 * s2
        )

      // (s1 + s2) * (s3 + s4) --> (s1 * s3) + (s1 * s4) + (s2 * s3) + (s2 * s4)
      case (Add(sub1, sub2), Add(sub3, sub4)) =>
        Add(
          Add(Mult(simplify(sub1), simplify(sub3)), Mult(simplify(sub1), simplify(sub4))),
          Add(Mult(simplify(sub2), simplify(sub3)), Mult(simplify(sub2), simplify(sub4)))
        )

      case (s1, s2) => Mult(s1, s2)
    }
  }
}


// 4.4.3
// a * 1 -> a ; a + 0 -> a
def optimize(e: Expr): Expr = e match {
  case Atom(a) => Atom(a)

  case Mult(e1, e2) => {
    (e1, e2) match {
      case (Atom(0), _) => Atom(0)
      case (_, Atom(0)) => Atom(0)
      case (Atom(1), _) => optimize(e2)
      case (_, Atom(1)) => optimize(e1)
      case (_, _) => Mult(optimize(e1), optimize(e2))
    }
  }


  case Add(e1, e2) => {
    (e1, e2) match {
      case (Atom(0), _) => optimize(e2)
      case (_, Atom(0)) => optimize(e1)
      case (_, _) => Add(optimize(e1), optimize(e2))
    }
  }
}




// 4.5 Matrix manipulation

// fiecare lista reprezinta o linie din matrice
type Matrix = List[List[Int]]

@tailrec def printLine(line: List[Int]): Unit = line match {
  case Nil => ()
  case x :: xs => {
    print(x)

    xs match {
      case Nil => ()
      case _ => print(", ")
    }
    printLine(xs)
  }
}

def printMatHelper(mat: Matrix): Unit = mat match {
  case Nil => println()
  case x :: xs => {
    print("\t[")
    printLine(x)
    print("]")

    xs match {
      case Nil => ()
      case _ => println(",")
    }

    printMatHelper(xs)
  }
}


def printMat(mat: Matrix): Unit = {
  print("Matrix = [\n")
  printMatHelper(mat)
  println("]\n")
}



// 4.5.1
def scalarProd(m: Matrix)(v: Int): Matrix = {
  m.map(
    line => line.map(el => el * v)
  )
}


// 4.5.2
// horizontal join -> concatenarea listelor de pe aceeasi linie
def hJoin_v1(m1: Matrix, m2: Matrix): Matrix = (m1, m2) match {
  case (Nil, Nil) => Nil
  /** opeatorii `:::` si `++` concateneaza doua liste **/
  case (x1 :: xs1, x2 :: xs2) => List(x1 ::: x2) ::: hJoin_v1(xs1, xs2)
  case (_, _) => throw new Exception("ERR: Matrices have different sizes!")
}


// 4.5.2
// horizontal join -> concatenarea listelor de pe aceeasi linie
def hJoin_v2(m1: Matrix, m2: Matrix): Matrix = {
  if (m1.length != m2.length || m1.head.length != m2.head.length) {
    throw new Exception("Err: Matricile au dimensiuni diferite")
  }

  /** opeatorii `:::` si `++` concateneaza doua liste **/
  m1.zip(m2).map { case (row1, row2) => row1 ::: row2 }
}



// 4.5.3
def vJoin(m1: Matrix, m2: Matrix) = {
  @tailrec def helper(mat1: Matrix, mat2: Matrix, acc: Matrix): Matrix = {
    (mat1, mat2) match {
      case (Nil, Nil) => acc
      case (Nil, x :: xs) => helper(Nil, xs, acc :+ x)
      case (x :: xs, _) => helper(xs, mat2, acc :+ x)
    }
  }

  if (m1.head.length != m2.head.length) {
    throw new Exception("Err: Matricile au numar diferite de coloane")
  }

  helper(m1, m2, Nil)
}



// 4.5.3
def matSum_v1(m1: Matrix, m2: Matrix): Matrix = {
  @tailrec def sumLines(line1: List[Int], line2: List[Int], acc: List[Int]): List[Int] = {
    (line1, line2) match
      case (Nil, Nil) => acc
      case (x1 :: xs1, x2 :: xs2) => sumLines(xs1, xs2, acc :+ (x1 + x2))
      case (_, _) => throw new Exception("Err: Matricile au dimensiuni diferite")
  }

  @tailrec def helper(mat1: Matrix, mat2: Matrix, acc: Matrix): Matrix = {
    (mat1, mat2) match
      case (Nil, Nil) => acc
      case (line1 :: rest1, line2 :: rest2) => helper(rest1, rest2, acc ++ List(sumLines(line1, line2, Nil)))
      case (_, _) => throw new Exception("Err: Matricile au dimensiuni diferite")
  }


  if (m1.length != m2.length && m1.head.length != m2.head.length) {
    throw new Exception("Err: Matricile au dimensiuni diferite")
  }

  helper(m1, m2, Nil)
}



// 4.5.3
def matSum_v2(m1: Matrix, m2: Matrix): Matrix = {
  if (m1.length != m2.length && m1.head.length != m2.head.length) {
    throw new Exception("Err: Matricile au dimensiuni diferite")
  }

  m1.zip(m2).map({
    case (line1, line) => line1.zip(line).map({
      case (el1, el2) => el1 + el2
    })
  })
}




/**         TESING         **/


// 4.1 Natural Number
val natZero: Nat = Zero
val natOne: Nat = Succ(Zero)
val natTwo: Nat = Succ(Succ(Zero))
val natThree: Nat = Succ(Succ(Succ(Zero)))
val natFour: Nat = Succ(natThree)
val natFive: Nat = Succ(natFour)

// 4.1.3
print(natZero)  ; println(" == " + toNat(0))
print(natOne)   ; println(" == " + toNat(1))
print(natTwo)   ; println(" == " + toNat(2))
print(natThree) ; println(" == " + toNat(3))
print(natFour)  ; println(" == " + toNat(4))
print(natFive)  ; println(" == " + toNat(5))


// 4.1.3
print(natZero)  ; println(" == " + NatToInt(natZero))
print(natOne)   ; println(" == " + NatToInt(natOne))
print(natTwo)   ; println(" == " + NatToInt(natTwo))
print(natThree) ; println(" == " + NatToInt(natThree))
print(natFour)  ; println(" == " + NatToInt(natFour))
print(natFive)  ; println(" == " + NatToInt(natFive))


// 4.1.2
// testing `add_v1`:
println("testing `add_v1`:")
print(0 + 0) ; print(" == ") ;  println(NatToInt(add_v1(toNat(0), toNat(0))))
print(5 + 0) ; print(" == ") ;  println(NatToInt(add_v1(toNat(5), toNat(0))))
print(0 + 5) ; print(" == ") ;  println(NatToInt(add_v1(toNat(0), toNat(5))))
print(2 + 4) ; print(" == ") ;  println(NatToInt(add_v1(toNat(2), toNat(4))))
print(4 + 2) ; print(" == ") ;  println(NatToInt(add_v1(toNat(4), toNat(2))))
print(18 + 24) ; print(" == ") ;  println(NatToInt(add_v1(toNat(18), toNat(24))))
print(24 + 18) ; print(" == ") ;  println(NatToInt(add_v1(toNat(24), toNat(18))))
// testing `add_v2`:
println("testing `add_v2`:")
print(0 + 0) ; print(" == ") ;  println(NatToInt(add_v2(toNat(0), toNat(0))))
print(5 + 0) ; print(" == ") ;  println(NatToInt(add_v2(toNat(5), toNat(0))))
print(0 + 5) ; print(" == ") ;  println(NatToInt(add_v2(toNat(0), toNat(5))))
print(2 + 4) ; print(" == ") ;  println(NatToInt(add_v2(toNat(2), toNat(4))))
print(4 + 2) ; print(" == ") ;  println(NatToInt(add_v2(toNat(4), toNat(2))))
print(18 + 24) ; print(" == ") ;  println(NatToInt(add_v2(toNat(18), toNat(24))))
print(24 + 18) ; print(" == ") ;  println(NatToInt(add_v2(toNat(24), toNat(18))))


// 4.1.3
// testing `multiply_v1`:
println("testing `multiply_v1`:")
print(0 * 0) ; print(" == ") ;  println(NatToInt(multiply_v1(toNat(0), toNat(0))))
print(5 * 0) ; print(" == ") ;  println(NatToInt(multiply_v1(toNat(5), toNat(0))))
print(0 * 5) ; print(" == ") ;  println(NatToInt(multiply_v1(toNat(0), toNat(5))))
print(2 * 4) ; print(" == ") ;  println(NatToInt(multiply_v1(toNat(2), toNat(4))))
print(4 * 2) ; print(" == ") ;  println(NatToInt(multiply_v1(toNat(4), toNat(2))))
print(18 * 24) ; print(" == ") ;  println(NatToInt(multiply_v1(toNat(18), toNat(24))))
print(24 * 18) ; print(" == ") ;  println(NatToInt(multiply_v1(toNat(24), toNat(18))))
// testing `multiply_v2`:
println("testing `multiply_v2`:")
print(0 * 0) ; print(" == ") ;  println(NatToInt(multiply_v2(toNat(0), toNat(0))))
print(5 * 0) ; print(" == ") ;  println(NatToInt(multiply_v2(toNat(5), toNat(0))))
print(0 * 5) ; print(" == ") ;  println(NatToInt(multiply_v2(toNat(0), toNat(5))))
print(2 * 4) ; print(" == ") ;  println(NatToInt(multiply_v2(toNat(2), toNat(4))))
print(4 * 2) ; print(" == ") ;  println(NatToInt(multiply_v2(toNat(4), toNat(2))))
print(18 * 24) ; print(" == ") ;  println(NatToInt(multiply_v2(toNat(18), toNat(24))))
print(24 * 18) ; print(" == ") ;  println(NatToInt(multiply_v2(toNat(24), toNat(18))))
print(24 * 18) ; print(" == ") ;  println(NatToInt(multiply_v1(toNat(24), toNat(18))))
// testing `multiply_v3`:
println("testing `multiply_v3`:")
print(0 * 0) ; print(" == ") ;  println(NatToInt(multiply_v3(toNat(0), toNat(0))))
print(5 * 0) ; print(" == ") ;  println(NatToInt(multiply_v3(toNat(5), toNat(0))))
print(0 * 5) ; print(" == ") ;  println(NatToInt(multiply_v3(toNat(0), toNat(5))))
print(2 * 4) ; print(" == ") ;  println(NatToInt(multiply_v3(toNat(2), toNat(4))))
print(4 * 2) ; print(" == ") ;  println(NatToInt(multiply_v3(toNat(4), toNat(2))))
print(18 * 24) ; print(" == ") ;  println(NatToInt(multiply_v3(toNat(18), toNat(24))))
print(24 * 18) ; print(" == ") ;  println(NatToInt(multiply_v3(toNat(24), toNat(18))))
// testing `multiply_v4`:
println("testing `multiply_v4`:")
print(0 * 0) ; print(" == ") ;  println(NatToInt(multiply_v4(toNat(0), toNat(0))))
print(5 * 0) ; print(" == ") ;  println(NatToInt(multiply_v4(toNat(5), toNat(0))))
print(0 * 5) ; print(" == ") ;  println(NatToInt(multiply_v4(toNat(0), toNat(5))))
print(2 * 4) ; print(" == ") ;  println(NatToInt(multiply_v4(toNat(2), toNat(4))))
print(4 * 2) ; print(" == ") ;  println(NatToInt(multiply_v4(toNat(4), toNat(2))))
print(18 * 24) ; print(" == ") ;  println(NatToInt(multiply_v4(toNat(18), toNat(24))))
print(24 * 18) ; print(" == ") ;  println(NatToInt(multiply_v4(toNat(24), toNat(18))))
print(3 * 4) ; print(" == ") ;  println(NatToInt(multiply_v4(toNat(3), toNat(4))))
print(4 * 2) ; print(" == ") ;  println(NatToInt(multiply_v4(toNat(4), toNat(3))))
print(15 * 1) ; print(" == ") ;  println(NatToInt(multiply_v4(toNat(15), toNat(1))))
print(1 * 15) ; print(" == ") ;  println(NatToInt(multiply_v4(toNat(1), toNat(15))))



// 4.5 Matrix manipulation
val mat1: Matrix = List(
  List(1, 2, 3),
  List(4, 5, 6),
  List(7, 8, 9),
)



val mat2: Matrix = (1 to 9).toList.grouped(3).toList

val mat3: Matrix = List(
  List(1, 2),
  List(3, 4)
)

val mat4: Matrix = List(
  List(5, 6),
  List(7, 8)
)

printMat(mat1)
printMat(mat2)
printMat(mat3)
printMat(mat4)


// 4.5.1
println("Scalar prod")
printMat(scalarProd(mat1)(0))
printMat(scalarProd(mat2)(-1))
printMat(scalarProd(mat3)(123))
printMat(scalarProd(mat4)(-12))


// 4.5.2
println("Horizontal Join")
printMat(hJoin_v1(mat1, mat2))
printMat(hJoin_v1(mat3, mat4))
printMat(hJoin_v2(mat1, mat2))
printMat(hJoin_v2(mat3, mat4))

// 4.5.2
println("Vertical Join")
printMat(vJoin(mat1, mat2))
printMat(vJoin(mat3, mat4))

// 4.5.2
println("Sum mat1 + mat2 = ")
printMat(matSum_v1(mat1, mat2))
println("Sum mat3 + mat4 = ")
printMat(matSum_v1(mat3, mat4))

println("Sum mat1 + mat2 = ")
printMat(matSum_v2(mat1, mat2))
println("Sum mat3 + mat4 = ")
printMat(matSum_v2(mat3, mat4))
