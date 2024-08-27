import Ord.LT
import Ord.GT
import Ord.EQ

import scala.annotation.tailrec

// 5.1 Variance

class Animal(name: String) {
  override def toString: String = name
}
case class Cat(name: String) extends Animal(name)
case class Dog(name: String) extends Animal(name)

/**   COVARIANTA    **/

// 5.1.1 We introduce a carrier class. Modify the type signature so that it allows for animal subtypes.
// initial: class Carrier[T <: Animal](val content: T)

/** covrariant in orice clasa sub tip a lui Animal **/
// `+T` contravariant in `T`
// <: Animal   orice clasa subtip a lui Animal
class Carrier[+T <: Animal](val content: T)

val catCarrier: Carrier[Cat] = new Carrier[Cat](Cat("Felix"))
val animalCarrier: Carrier[Animal] = catCarrier

/**
 * nu o sa merga: dogCarrier este covariant in Dog
 * putem vizualiza ierarhia de clase astfle
 *    +------------ Animal ------------ +
 *    |                                 |
 *   Dog                               Cat
 *
 *
 *  Covariant defineste o relatie intre Dog si Animal,
 *  nu exista nicio relatie intre Dog si Cat
**/
// val dogCarrier: Carrier[Dog] = new Carrier[Dog](Cat("Merv"))




/**   CONTRAVARIANTA    **/

// `-T` contravarianta in T
// <: Animal   orice clasa subtip a lui Animal
class Vet[-T <: Animal] {
  def treat(patient: T): String = {
    "Can treat " + patient.toString
  }
}



// 5.1.3 Evaluate the following and give an explanation as to why they work or not.
val generalVet: Vet[Animal] = new Vet[Animal]
val dogVet: Vet[Dog] = generalVet

/**
 * nu vor functiona
 *
 * dogVet este instanta a clasei `Vet[Dog]`
 * Un veterinat de caini este contravariant in Animal
 * (un veterinat de caini stie sa trateze si Animale, nu si pisici)
 *
 * dogVet.treat(Cat("Bob"))
 *
 * Un veterinar de caini si unul de pisici sunt doua lucruri (instante) total diferite
 * val catVet: Vet[Cat] = new Vet[Dog]
*/


enum Ord:
  case LT, EQ, GT

// `T` = invariant
// 5.1.4. Implement a comparator class that compares animals by their name.
class Comparator[T <: Animal] {
  def compare(o1: T, o2: T): Ord = {
    val name1 = o1.asInstanceOf[Animal].toString
    val name2 = o2.asInstanceOf[Animal].toString

    if (name1 == name2) Ord.EQ
    else if (name1 < name2) Ord.LT
    else Ord.GT
  }
}

// 5.1.5
def detFirst_v1(lst: List[Animal]): Animal = {
  val comparator = new Comparator[Animal]
  lst.reduceLeft((a, b) => if (comparator.compare(a, b) == Ord.LT) a else b)
}

// 5.1.5
def detFirst_v2(lst: List[Animal]): Animal = {
  val comparator = new Comparator[Animal]
  lst.reduceRight((a, b) => if (comparator.compare(a, b) == Ord.LT) a else b)
}



// 5.1.5
// sorteaza descrescator o lista de animale dupa nume
def sortAnimalsDesc(lst: List[Animal]): List[Animal] = {
  val comparator = new Comparator[Animal]
  lst.sortWith((a, b) => comparator.compare(a, b) == Ord.GT)
}

def detFirst_v3(lst: List[Animal]): Animal = {
  sortAnimalsDesc(lst).head
}




// 5.1.6
def apply(g: List[Animal] => Animal)(lst: List[Animal]): Animal = {
  g(lst)
}



// 5.2. Implicits and extensions

trait BoolExpr {

  // 5.2.4. Implement a method in trait BoolExpr, which returns a list of variables in the expression.
  def getVars: List[String] = {
    // helper function
    def varsExpr(e: BoolExpr): List[String] = {
      e match {
        case Symbol(s) => List(s)
        case True | False => Nil
        case And(e1, e2) => varsExpr(e1) ++ varsExpr(e2)

        case Or(e1, e2) => varsExpr(e1) ++ varsExpr(e2)

        case Not(e) => varsExpr(e)
      }
    }

    varsExpr(this)
  }


  // 5.2.5 evaluates the epxression, with respect to the stored values given
  def eval(store: Map[String, Boolean]): Boolean = {
    // helper function
    def evaluationHelper(expr: BoolExpr): Boolean = expr match {
      case True => true
      case False => false

      case Symbol(key) => {
        if (store.contains(key) == false) throw new Exception(s"Err: nu exista simbolul `$key`")

        store.get(key) match {
          case Some(value) => value
          case None => throw new Exception(s"Err: nu exista simbolul `$key`")
        }
      }

      case And(e1, e2) => evaluationHelper(e1) && evaluationHelper(e2)
      case Or(e1, e2) => evaluationHelper(e1) || evaluationHelper(e2)
      case Not(e) => !evaluationHelper(e)
    }

    evaluationHelper(this)
  }

}


// 5.2.1. Implement the type BoolExpr, which contains the following constructors:
object True extends BoolExpr
object False extends BoolExpr
case class Symbol(s: String) extends BoolExpr
case class And(e1: BoolExpr, e2: BoolExpr) extends BoolExpr
case class Or(e1: BoolExpr, e2: BoolExpr) extends BoolExpr
case class Not(e: BoolExpr) extends BoolExpr

// 5.2.2
implicit def boolToExpr(b: Boolean): BoolExpr = {
  if (b == true) True
  else False
}

val e = And(Not(Or(Symbol("x"), Symbol("y"))), And(True, Symbol("z")))


// 5.2.3: implementing the following operators: `&&`, `||`, `~`
extension(e: BoolExpr) {
  def &&(e2: BoolExpr): BoolExpr = And(e, e2)

  def ||(e2: BoolExpr): BoolExpr = Or(e, e2)

  def ~(): BoolExpr = Not(e)
  def unary_! : BoolExpr = Not(e)
}



// 5.2.6
class Solver(formula: BoolExpr){
  type Store = Map[String,Boolean]

  // genereaza toate posibilitatile pentru o expresie data
  def interpretations: List[Store] = {
    def generate(store: Store, vars: List[String]): List[Store] = vars match {
      case Nil => List(store)
      case v :: rest =>
        generate(store + (v->true), rest) ++ generate(store + (v->false), rest)
    }

    generate(Map.empty, formula.getVars.distinct)
  }

  // itereaza peste toate store -urile si il intoarce pe primul care respecta expresia
  def solve: Option[Store] = {
    // returneaza primul element din list pe care il gaseste
    // element, care respecta o conditie data
    interpretations.find(formula.eval)
  }
}

