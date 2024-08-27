import scala.annotation.tailrec
import scala.util.{Try, Success, Failure}


/** 4.1 tip de date NAT, numere naturale >= 0  */
trait Nat
case object Zero extends Nat
case class Succ(x: Nat) extends Nat


/**
 * 4.1.1 adunarea a doua Nat-uri (numere naturale)
 * @param x
 * @param y
 * @return    x + y
 */
def add(x: Nat, y: Nat): Nat = {

  @tailrec
  def helper(a: Nat, b: Nat, acc: Nat): Nat = {
    (a, b) match {
      case (Zero, Zero) => acc
      case (Succ(beforeA), Zero) => helper(beforeA, b, Succ(acc))
      case (Zero, Succ(beforeB)) => helper(a, beforeB, Succ(acc))
      case (Succ(beforeA), Succ(beforeB)) => helper(beforeA, beforeB, Succ(Succ(acc)))
    }
  }


  helper(x, y, Zero)

}


/**
 * 4.1.2 inmultirea a doua Nat-uri (numere naturale)
 *
 * x * y = 1 + 1 + 1 + ... (de `x * y` ori)
 * x * y = y + y + y + . .... (de `x` ori)
 * x * y = x + x + x + .... (de `y` ori)
 * @param x
 * @param y
 * @return    x * y
 */
def multiply(x: Nat, y: Nat): Nat = {

  /**
   * il adunam de `y` de x ori
   */
  @tailrec
  def helper(a: Nat, acc: Nat): Nat = {
    a match {
      case Zero => acc
      case Succ(beforeA) => helper(beforeA, add(acc, y))
    }
  }


  helper(x, Zero)
}











def multiply_method1(x: Nat, y: Nat): Nat = {

  /**
   * il adunam de `y` de x ori
   */
  @tailrec
  def helper(a: Nat, acc: Nat): Nat = {
    a match {
      case Zero => acc
      case Succ(beforeA) => helper(beforeA, add(acc, y))
    }
  }


  helper(x, Zero)
}




def multiply_method2(x: Nat, y: Nat): Nat = {

  /**
   * il adunam de `x` de y ori
   */
  @tailrec
  def helper(a: Nat, acc: Nat): Nat = {
    a match {
      case Zero => acc
      case Succ(beforeA) => helper(beforeA, add(acc, x))
    }
  }


  helper(y, Zero)
}





def multiply_method3(x: Nat, y: Nat): Nat = {

  /**
   * adunam `1` de n * m ori
   */
  @tailrec
  def helper(a: Nat, b: Nat, acc: Nat): Nat = {
    (a, b) match {
      case (Zero, _) | (_, Zero) => Zero
      case (Succ(Zero), Succ(Zero)) => acc    // 1 * 1
      case (Succ(beforeA), Succ(Zero)) => helper(beforeA, y, Succ(acc))
      case (_, Succ(beforeB)) => helper(a, beforeB, Succ(acc))
    }
  }

  helper(x, y, Succ(Zero))
}





/**
 * 4.1.3
 * @param x
 * @return
 */
def toNat(x: Int): Nat = {

  @tailrec
  def helper(a: Int, acc: Nat): Nat = {
    if (a < 0) Zero
    else if (a == 0) acc
    else helper(a - 1, Succ(acc))
  }

  if (x < 0) {
    println("Nu putem face casting de la un numar negativ la NAT")
    Zero
  } else {
    helper(x, Zero)
  }
}


/**
 *
 * @param x
 * @return    un numar natural
 */
def natToInt(x: Nat): Int = {
  
  @tailrec
  def helper(a: Nat, acc: Int): Int = {
    a match {
      case Zero => acc
      case Succ(beforeA) => helper(beforeA, acc + 1)
    }
  }
  
  
  helper(x, 0)
}







/** 4.2 Option (un fel de try-except) */

/**
 * 4.2.1
 * @param t
 * @param c
 * @return
 */
def realrealtrycatch(t: => Option[Int], c: => Int): Int = {
  Try(t) match {
    case Success(Some(value)) => value // Dacă t este Some, returnăm valoarea
    case _ => c // Altfel, apelăm funcția de catch c
  }
}


/**
 * 4.2.2
 * @param x   un numar intreg
 * @return    Nat(x), Some, daca x >= 0
 *            (), Nonen, daca x < 0
 */
def toNatOpt(x: Int): Option[Nat] = {
  if (x < 0) None
  else Some(toNat(x))
}


/**
 * 4.2.3
 * aduagam doua numere naturale + Option (Some/None)
 */
def addOpt(x: Option[Nat], y: Option[Nat]): Option[Nat] = {

  /**
   * helper-ul pt add ramane neschimbat
   */
  @tailrec
  def helper(a: Nat, b: Nat, acc: Nat): Nat = {
    (a, b) match {
      case (Zero, Zero) => acc
      case (Succ(beforeA), Zero) => helper(beforeA, b, Succ(acc))
      case (Zero, Succ(beforeB)) => helper(a, beforeB, Succ(acc))
      case (Succ(beforeA), Succ(beforeB)) => helper(beforeA, beforeB, Succ(Succ(acc)))
    }
  }


  val acc: Nat = Zero

  /** putem sa adunam doar numarul NAT
  * (x, y) match {
  *  case (None, None) => None
  *  case (None, Some(natY)) => Some(helper(Zero, natY, acc))
  *  case (Some(natX), None) => Some(helper(natX, Zero, acc))
  *  case (Some(natX), Some(natY)) => Some(helper(natX, natY, acc))
  * }
  */

  (x, y) match {
     case (None, _) | (_, None) => None
     case (Some(natX), Some(natY)) => Some(helper(natX, natY, acc))
  }
}






/** 4.3 Binary Trees */
trait BTree
case object EmptyTree extends BTree
case class Node(value: Int, left: BTree, right: BTree) extends BTree





/**
 * adauga o valoare in arborele binar
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



/**
 * 4.3.1
 * @param tree    arborele binar
 * @return        inalimea arborerului
 *                h(tree) = 1 + max(h(tree.left), h(tree.right))
 */
def depth(tree: BTree): Int = {
  tree match {
    case EmptyTree => 0
    case Node(value, leftTree, rightTree) => {
      val leftHeight: Int = depth(leftTree)
      val rightHeight: Int = depth(rightTree)

      if (leftHeight > rightHeight) leftHeight + 1
      else rightHeight + 1
    }
  }
}


/**
 * 4.3.2
 *
 * @param tree    arborele binar
 * @return        numarul de noduri ale arborelui binar
 *                subtree(tree) = 1 + subtree(tree.left) + subtree(tree.right)
 */
def subtree(tree: BTree): Int = {
  tree match {
    case EmptyTree => 0
    case Node(value, leftTree, rightTree) => {
      val nrNodesLeft: Int = subtree(leftTree)
      val nrNodesRight: Int = subtree(leftTree)

      nrNodesLeft + nrNodesRight + 1
    }
  }
}


/**
 * 4.3.2
 *
 * @param tree    arborele binar
 * @return        numarul de noduri cu numar par (even) de copii
 */
def evenChildCount(tree: BTree): Int = {
  tree match {
    case EmptyTree => 0
    case Node(value, leftTree, rightTree) => {
      val nrChildren: Int = subtree(tree) - 1

      val nrNodesLeft: Int = evenChildCount(leftTree)
      val nrNodesRight: Int = evenChildCount(rightTree)

      if (nrChildren % 2 == 0) nrNodesLeft + nrNodesRight + 1
      else nrNodesLeft + nrNodesRight
    }
  }
}


/**
 * 4.3.3
 *
 * @param tree    arborele binar
 * @return        elemenentele arborelui, retinute intr-o lista
 *                arborele este parcurs in `preordine`: R-S-D
 *                Radacina - Stanga - Dreapta
 */
def flatten_preordine(tree: BTree): List[Int] = {

  tree match {
    case EmptyTree => List()
    case Node(value, leftTree, rightTree) => {

      val leftList: List[Int] = flatten_preordine(leftTree)
      val rightList: List[Int] = flatten_preordine(rightTree)

      // preordine: Radacina - Stanga - Dreapta
      List(value) ::: leftList ::: rightList
    }
  }
}




/**
 * 4.3.3
 *
 * @param tree    arborele binar
 * @return        elemenentele arborelui, retinute intr-o lista
 *                arborele este parcurs in `preordine`: S-R-D
 *                Stanga - Radacina - Dreapta
 */
def flatten_inordine(tree: BTree): List[Int] = {

  tree match {
    case EmptyTree => List()
    case Node(value, leftTree, rightTree) => {

      val leftList: List[Int] = flatten_inordine(leftTree)
      val rightList: List[Int] = flatten_inordine(rightTree)

      // preordine: Stanga - Radacian - Dreapta
      leftList ::: List(value) ::: rightList
    }
  }
}





/**
 * 4.3.3
 *
 * @param tree    arborele binar
 * @return        elemenentele arborelui, retinute intr-o lista
 *                arborele este parcurs in `preordine`: R-D-S
 *                Radacina - Dreapta - Stanga
 */
def flatten_postordine(tree: BTree): List[Int] = {

  tree match {
    case EmptyTree => List()
    case Node(value, leftTree, rightTree) => {

      val leftList: List[Int] = flatten_postordine(leftTree)
      val rightList: List[Int] = flatten_postordine(rightTree)

      // preordine: Stanga - Radacian - Dreapta
      leftList ::: rightList ::: List(value)
    }
  }
}


/**
 * 4.3.3
 *
 * @param tree    arborele binar
 * @return        elemenentele arborelui, retinute intr-o lista
 */
def flatten(tree: BTree): List[Int] = {
  flatten_preordine(tree)
}


/**
 * 4.3.5
 * @param tree    arborele binar
 * @param cond    conditia de verificare
 * @return        numarul de noduri care verifica conditia
 */
def countNodes(tree: BTree, cond: Int => Boolean): Int = {
  tree match {
    case EmptyTree => 0
    case Node(value, leftTree, rightTree) => {

      val nrNodesLeft: Int = countNodes(leftTree, cond)
      val nrNodesRight: Int = countNodes(rightTree, cond)

      if (cond(value)) nrNodesLeft + nrNodesRight + 1
      else nrNodesLeft + nrNodesRight
    }
  }
}


/**
 * 4.3.6
 * @param tree    arborele binar
 * @return        simetricul arborelui binar
 */
def mirror(tree: BTree): BTree = {
  tree match {
    case EmptyTree => EmptyTree
    case Node(value, leftTree, rightTree) => {
      Node(value, rightTree, leftTree)
    }
  }
}

/**
 * 4.3.7
 * concatenam doi arbori binari
 * daca valori sunt distincte, vom returna un arborele
 * altfel, `None`
 */
def append(tree1: BTree, tree2: BTree): Option[BTree] = {


  def helper(idx: Int, l: List[Int], tree: BTree): Option[BTree] = {
    if (idx >= l.length) Some(tree)
    else {
      if (!isInTree(tree, l(idx))) None
      else helper(idx + 1, l, addValInTree(tree, l(idx)))
    }
  }

  helper(0, flatten(tree1) ::: flatten(tree2), EmptyTree)
}



/** 4.4 Expression evaluation */
trait Expr
case class Atom(a: Int) extends Expr
case class Add(e1: Expr, e2: Expr) extends Expr
case class Mult(e1: Expr, e2: Expr) extends Expr

/**
 * 4.4.1   avalueaza expreisa
 */
def evaluate(e: Expr): Int = {
  e match {
    case Atom(a) => a
    case Add(e1, e2) => evaluate(e1) + evaluate(e2)
    case Mult(e1, e2) => evaluate(e1) * evaluate(e2)
  }
}



/**
 * 4.4.2    simplifica expresia (elimina parantezele)
 * a * (b + c) = a * b + a * c
 * */
def simplify(e: Expr): Expr = {
  e match {

    case Atom(el) => Atom(el)

    case Add(e1, e2) =>
      // e1 + e2
      Add(simplify(e1), simplify(e2))

    case Mult(Atom(a), Add(e1, e2)) =>
      // a * (e1 + e2) = a * e1 + a * e2
      Add(
        Mult(Atom(a), simplify(e1)),
        Mult(Atom(a), simplify(e2))
      )

    case Mult(Add(e1, e2), Atom(a)) =>
      // (e1 + e2) * a = e1 * a + e2 * a
      Add(
        Mult(Atom(a), simplify(e1)),
        Mult(Atom(a), simplify(e2))
      )

    case Mult(e1, e2) => Mult(simplify(e1), simplify(e2))
  }
}





/**
 * 4.4.3    optimizeaza exprsia
 * a * 1 = a
 * a * 0 = 0
 * */
def optimize(e: Expr): Expr = {
  e match {

    case Atom(a) => Atom(a)

    case Add(e1, e2) => {

      val opt1: Expr = optimize(e1)
      val opt2: Expr = optimize(e2)

      (opt1, opt2) match {
        case (Atom(a), Atom(b)) => Atom(a + b)
        case (Atom(0), expr) => expr
        case (expr, Atom(0)) => expr
        case _ => Add(opt1, opt2)
      }
    }

    case Mult(e1, e2) => {

      val opt1: Expr = optimize(e1)
      val opt2: Expr = optimize(e2)

      (opt1, opt2) match {
        case (Atom(a), Atom(b)) => Atom(a * b)
        case (Atom(0), _) => Atom(0)
        case (_, Atom(0)) => Atom(0)
        case (Atom(1), expr) => expr
        case (expr, Atom(1)) => expr
        case (Atom(-1), expr) => Mult(Atom(-1), expr)   // probabil nu face nimic
        case (expr, Atom(-1)) => Mult(Atom(-1), expr)   // probabil nu face nimic
        case _ => Mult(opt1, opt2)
      }
    }
  }
}






/** 4.5 Matrix manipulation */
type Matrix = List[List[Int]]


/**
 * 4.5.1    inmultim fiecare numar din matrice cu un scalar
 * */
def scalarProd(m: Matrix)(v: Int): Matrix = {
  m.map(row => row.map(el => el * v))
}


/**
 * 4.5.2      contcatenam a doua matrice in dreapta primei
 */
def hJoin(m1: Matrix, m2: Matrix): Matrix = {

  if (m1.length != m2.length) {
    println("Ambele matrici trebuie sa aiba acelasi numar de linii")
    throw new Exception()
  }


  val height: Int = m1.length

  @tailrec
  def column_helper(row: Int, idx1: Int, idx2: Int, acc: List[Int]): List[Int] = {
    if (idx1 >= m1(row).length && idx2 >= m2(row).length) acc
    else if (idx1 < m1(row).length) column_helper(row, idx1 + 1, idx2, acc :+ m1(row)(idx1))
    else column_helper(row, idx1, idx2, acc :+ m2(row)(idx2))
  }


  @tailrec
  def row_helper(idx: Int, acc: Matrix): Matrix = {
    if (idx >= height) acc
    else row_helper(idx + 1, acc :+ column_helper(idx, 0, 0, List()))
  }

  row_helper(0, List())
}


/**
 * 4.5.2      contcatenam a doua matrice dedesubtul primei
 */
def vJoin(m1: Matrix, m2: Matrix): Matrix = {

  if (m1.head.length != m2.head.length) {
    println("Matricile trebuie sa aiba acelasi numar de coloane")
    throw new Exception()
  }


  def helper(idx1: Int, idx2: Int, acc: Matrix): Matrix = {
    if (idx1 >= m1.length && idx2 >= m2.length) acc
    else if (idx1 < m1.length) helper(idx1 + 1, idx2, acc :+ m1(idx1))
    else helper(idx1, idx2 + 1, acc :+ m2(idx2))
  }


  helper(0, 0, List())
}



/**
 * 4.5.3    adunarea a doua matrici
 * */
def matSum(m1: Matrix, m2: Matrix): Matrix = {
  if (m1.length != m2.length && m1.head.length != m2.head.length) {
    println("Matrici invalide: ambele matrici trebuie sa aiba aceleasi dimensiuni")
    throw new Exception("Matrici invalide: ambele matrici trebuie sa aiba aceleasi dimensiuni");
  }


  // `zip` = combina doua colectii intr-un singura de perechi
  // `zipped` = returneaza un obiect care poate `itera` (`Iterable`) simultan peste ambele colectii
  (m1, m2).zipped.map((row1, row2) => (row1, row2).zipped.map(_ + _))
}



/** testare 4.1 tipul NAT  */
val x: Nat = Zero
val y: Nat = Succ(Succ(Succ(Zero)))
println("x = " + natToInt(x))
println("y = " + natToInt(y))


val x: Nat = toNat(1)
val y: Nat = toNat(12)
println("x = " + natToInt(x))
println("y = " + natToInt(y))
println("x + y = " + natToInt(add(x, y)))


val x: Nat = toNat(109)
val y: Nat = toNat(11)
println("x = " + natToInt(x))
println("y = " + natToInt(y))
println("x + y = " + natToInt(add(x, y)))



val x: Nat = toNat(0)
val y: Nat = toNat(11)
println("x = " + natToInt(x))
println("y = " + natToInt(y))
println("x * y = " + natToInt(multiply(x, y)))
println("x * y = " + natToInt(multiply_method1(x, y)))
println("x * y = " + natToInt(multiply_method2(x, y)))
println("x * y = " + natToInt(multiply_method3(x, y)))


val x: Nat = toNat(11)
val y: Nat = toNat(0)
println("x = " + natToInt(x))
println("y = " + natToInt(y))
println("x * y = " + natToInt(multiply(x, y)))
println("x * y = " + natToInt(multiply_method1(x, y)))
println("x * y = " + natToInt(multiply_method2(x, y)))
println("x * y = " + natToInt(multiply_method3(x, y)))


val x: Nat = toNat(1)
val y: Nat = toNat(22)
println("x = " + natToInt(x))
println("y = " + natToInt(y))
println("x * y = " + natToInt(multiply(x, y)))
println("x * y = " + natToInt(multiply_method1(x, y)))
println("x * y = " + natToInt(multiply_method2(x, y)))
println("x * y = " + natToInt(multiply_method3(x, y)))


val x: Nat = toNat(22)
val y: Nat = toNat(1)
println("x = " + natToInt(x))
println("y = " + natToInt(y))
println("x * y = " + natToInt(multiply(x, y)))
println("x * y = " + natToInt(multiply_method1(x, y)))
println("x * y = " + natToInt(multiply_method2(x, y)))
println("x * y = " + natToInt(multiply_method3(x, y)))

val x: Nat = toNat(4)
val y: Nat = toNat(5)
println("x = " + natToInt(x))
println("y = " + natToInt(y))
println("x * y = " + natToInt(multiply(x, y)))
println("x * y = " + natToInt(multiply_method1(x, y)))
println("x * y = " + natToInt(multiply_method2(x, y)))
println("x * y = " + natToInt(multiply_method3(x, y)))

val x: Nat = toNat(5)
val y: Nat = toNat(4)
println("x = " + natToInt(x))
println("y = " + natToInt(y))
println("x * y = " + natToInt(multiply(x, y)))
println("x * y = " + natToInt(multiply_method1(x, y)))
println("x * y = " + natToInt(multiply_method2(x, y)))
println("x * y = " + natToInt(multiply_method3(x, y)))




/** testare 4.2 Option */


var x: Nat = null


toNatOpt(-1) match {
  case None => println("NAT nu poate primi un numar negativ `-1`")
  case Some(nat) => x = nat;
}

val result: String = toNatOpt(5) match {
  case Some(natValue) => s"Numărul natural este: $natValue"
  case None => "Numărul nu este un număr natural"
}

println(result)




val res: String = addOpt(toNatOpt(1), toNatOpt(2)) match {
  case None => "Operatie invalida"
  case Some(nat) => "adunare = " + natToInt(nat)
}
println(res)



val res: String = addOpt(toNatOpt(0), toNatOpt(-2)) match {
  case None => "Operatie invalida"
  case Some(nat) => "adunare = " + natToInt(nat)
}
println(res)


val res: String = addOpt(toNatOpt(-1), toNatOpt(2)) match {
  case None => "Operatie invalida"
  case Some(nat) => "adunare = " + natToInt(nat)
}
println(res)


val res: String = addOpt(toNatOpt(-2), toNatOpt(-1)) match {
  case None => "Operatie invalida"
  case Some(nat) => "adunare = " + natToInt(nat)
}
println(res)


if val res: String



/**
 * ce a predat la laborator
 *
 *  trait = tipul de date
 *  object = instanta unica
 *  case class = instanta imutabila (odata creata, nu vrem sa le modificam ce au inauntru)
 *
 *  Nat -> Succ(), Zero
 * 1 / 0 -> None
 * 4 / 2 -> Some(2)
 *
 */