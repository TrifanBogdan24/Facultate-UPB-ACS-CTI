val list01: List[Int] = Nil                  // Empty List
val list02: List[Int] = List.empty           // Empty List
val list03 = List.empty[Int]                 // Empty List
val list04 = List(1, 2, 3, 4)                // List Constructor
val list05 = 1 :: 2 :: 3 :: 4 :: Nil         // Cons
val list06 = 1 :: List(2, 3, 4)              // Cons and List Constructor
val list07 = 101 :: List(1, 2, 3)            // [101, 1, 2, 3] (Cons operator)
val list08 = 101 +: List(1, 2, 3)            // [101, 1, 2, 3]
val list09 = List(1, 2, 3) :+ 101            // [1, 2, 3, 101]
val list10 = List(1, 2) ::: List(3, 4)       // [1, 2, 3, 4]
val list11 = List(1, 2) ++ List(3, 4)        // [1, 2, 3, 4]

val first = list11(0)                         // indexing



def sumEls(list: List[Int]): Int = {

  @tailrec def helper(l: List[Int], acc: Int): Int = l match
    case Nil => acc
    case x :: xs => helper(xs, acc + x)

  helper(list, 0)
}

import scala.annotation.tailrec

extension[A] (list: List[A]) {


  /**
   * @param z     valoarea initiala
   * @param func  functie intre `acc, value` (acumulator si valoare curenta)
   */
  def myFoldLeft[B](init: B)(func: (B, A) => B): B = {
    @tailrec def helper(l: List[A], acc: B): B = l match {
      case Nil => acc
      case x :: xs => helper(xs, func(acc, x))
    }
    helper(list, init)
  }

  /**
   * @param z    valoarea initiala
   * @param func functie intre `value, acc` (valoarea curenta si acumulator)
   */
  def myFoldRight[B](init: B)(func: (A, B) => B): B = {
    def helper(l: List[A]): B = l match {
      case Nil => init
      case x :: xs => func(x, helper(xs))
    }

    helper(list)
  }
}


val x: Int = List(1, 2, 3, 4).head            // 1
val xs: List[Int] = List(1, 2, 3, 4, 5).tail  // [2, 3, 4]

val list12 = List(1, 2, 3, 4, 5).take(2)        // [1, 2]
val list13 = List(1, 2, 3, 4, 5).drop(2)        // [3, 4, 5]
val list14 = List(1, 2, 3, 4, 5).takeRight(2)   // [4, 5]
val list15 = List(1, 2, 3, 4, 5).dropRight(2)   // [1, 2, 3]

val prod_l1: Int = List(1, 2, 3, 4).foldLeft(1)((acc, value) => acc * value)    // 24
val prod_r1: Int = List(1, 2, 3, 4).foldRight(1)((value, acc) => acc * value)   // 24

val sum_l1: Int = List(1, 2, 3, 4).foldLeft(0)(_ + _)   // 10
val sum_r1: Int = List(1, 2, 3 ,4).foldRight(0)(_ + _)  // 10

val reverse_l1: List[Int] =   // [4, 3, 2, 1]
  List(1, 2, 3, 4)
  .foldLeft(List.empty)((acc: List[Int], value: Int) => value +: acc)


val reverse_l2: List[Int] =   // [4, 3, 2, 1]
  List(1, 2, 3, 4)
  .foldRight(List.empty)((value: Int, acc:List[Int]) => acc :+ value)


val sum: Int = List(1, 2, 3, 4).fold(0)(_ + _)    // 10


val all_els = List(1, 2, 3, 4).filter(_ => true)      // [1, 2, 3, 4]
val none_els = List(1, 2, 3, 4).filter(_ => false)    // []

val odd_nums1 = List(1, 2, 3, 4).filter(x => x % 2 == 1)            // [1, 3]
val even_nums1 = List(1, 2, 3, 4).filter((x: Int) => x % 2 == 0)    // [2, 3]


val odd_nums2 = List(1, 2, 3, 4).filterNot(_ % 2 == 0)    // [1, 3]


val exp2 = List(1, 2, 3, 4).map((x: Int) => x * x)
// [1, 3, 9, 16]

                  /** list of tuples **/
val prods = List((1, 10), (5, 10), (2, 20)).map(x => x._1 * x._2)
// [10, 50, 40]

                    /** list of tuples **/     /** destructuring the tuple using `case` **/
val sums = List((1, 10), (5, 10), (2, 20)).map( { case (a, b) => a + b } )
// [11, 15, 22]




val idx: List[(Int, Int)] = List(1, 2, 3, 4).zipWithIndex
// [(1, 0), (2, 1), (3, 2), (4, 3)]

val pairs: List[(Int, String)] = List(1, 2, 3).zip("one" :: "two" :: "three" :: Nil)
// [(1, one), (2, two), (3, three)]




val pair: (Int, Double) = (1, 1.0)
val first_p = pair._1   // 1
val second_p = pair._2  // 1.0

val triplet: (String, List[Int], Double) = ("George", 1 :: 2 :: Nil, 1e-3)
val first_t = triplet._1      // George
val second_t = triplet._2     // [1, 2]
val third_t = triplet._3      // 0.001




// `type` alias (nume asociate unor tipuri de date deja existente)
type Matrix = List[List[Int]]
type GraphAdjLists = List[(Int, List[Int])]
type Name = String
type PhoneNumber = String
type PhoneBook = List[(Name, PhoneNumber)]      // lista de tupluri



// Evaluarea expresiilor matematice de baza
trait Expr
// putem face `pattern matching` pe `case class`
case class Atom(a: Int) extends Expr
case class Add(e1: Expr, e2: Expr) extends Expr
case class Mult(e1: Expr, e2: Expr) extends Expr


def evaluate(e: Expr): Int = e match {
  // `pattern matching`
  case Atom(a) => a
  case Add(e1, e2) => evaluate(e1) + evaluate(e2)
  case Mult(e1, e2) => evaluate(e1) * evaluate(e2)
}


/** Expresii lambda (functii anonime)
 * (param1: Type1, param2: Type2, param3: Type3) => expressions
 */

val lambda_cond = (name: String, grade: Int) => grade >= 5

val doubler1 = (x: Int, y: Int) => x + y

val addTuple1 = (t: (Int, Int)) => t._1 + t._2


val tuplesList = List((1, 2), (3, 4), (5, 6))
val sums1 = tuplesList.map((t: (Int, Int)) => t._1 + t._2)
val sums2 = tuplesList.map { case (x: Int, y: Int) => x + y }


val applyFunction1 = (f: Int => Int, x: Int) => f(x)






// Functor
trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  def apply[F[_]](implicit functor: Functor[F]): Functor[F] = functor

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit val listFunctor: Functor[List] = new Functor[List] {
    def fmap[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }
}


object Monad_V1 {
  // Problema: `Monad` pentru propagarea erorilor generate de impartirea cu 0
  case class Monad[A](value: Option[A]) {
    def map[B](f: A => B): Monad[B] = Monad(value.map(f))

    def bind[B](f: A => Monad[B]) = Monad(value.flatMap(a => f(a).value))
  }


  // companion object
  object Monad {
    def pure[A](a: A): Monad[A] = Monad(Some(a))
  }
}







object Monad_V2_With_Steps {
  // Problema: `Monad` pentru propagarea erorilor generate de impartirea cu 0
  // In plus, pastram un istoric al calculelor facute pe parcurs

  case class Monad[A](value: Option[A], steps: List[String] = List.empty) {
    def map[B](f: A => B): Monad[B] = Monad(value.map(f), steps)

    def bind[B](f: A => Monad[B]) = value match
      case Some(v) => Monad(f(v).value, steps ++ f(v).steps)
      case None => Monad(None, steps)
  }


  // companion object
  object Monad {
    def pure[A](a: A): Monad[A] = Monad(Some(a))
  }
}

