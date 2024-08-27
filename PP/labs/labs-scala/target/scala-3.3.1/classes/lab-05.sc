import scala.annotation.tailrec
// Laboratorul 5


// 5.1 Variance
class Animal(name: String) {
  override def toString: String = name
}
case class Cat(name: String) extends Animal(name)
case class Dog(name: String) extends Animal(name)

// Carrier este invariant in T
// T este coveriant, un subtip al lui Animal
class Carrier[+T <: Animal](val content: T)


// nu o sa mearga
// compilatorul nu spune sa folosim `+T` la Carrier
val catCarrier: Carrier[Cat] = new Carrier[Cat](Cat("Felix"))
val animalCarrier: Carrier[Animal] = catCarrier

// conversia de la Cat la Dog duce la eroare
// val dogCarrier: Carrier[Dog] = new Carrier[Dog](Cat("Merv"))
val dogCarrier: Carrier[Dog] = new Carrier[Dog](Dog("Merv"))

// contravariant:
class Vet[-T <: Animal] {
  def treat(patient: T): String = {
    "Can treat " + patient.toString
  }
}


val generalVet: Vet[Animal] = new Vet[Animal]
val dogVet: Vet[Dog] = generalVet

// nu e bine
//dogVet.treat(Cat("Bob"))

// val catVet: Vet[Cat] = new Vet[Dog]



enum Ord:
  case LT, EQ, GT

class Comparator[T] {
  def compare(o1: T, o2: T): Ord = {
    // Vom presupune că obiectele sunt instanțe ale unor clase care au un câmp nume de tip String
    val name1 = o1.asInstanceOf[Animal].toString
    val name2 = o2.asInstanceOf[Animal].toString
    if (name1 < name2) Ord.LT
    else if (name1 > name2) Ord.GT
    else Ord.EQ
  }
}

def detFirst(lst: List[Animal]): Animal = {
  val comparator = new Comparator[Animal]

  @tailrec
  def detFirstRec(remaining: List[Animal], firstAnimal: Animal): Animal = {
    remaining match {
      case Nil => firstAnimal
      case head :: tail =>
        val newFirstAnimal = if (comparator.compare(head, firstAnimal) == Ord.LT) head else firstAnimal
        detFirstRec(tail, newFirstAnimal)
    }
  }

  detFirstRec(lst.tail, lst.head)
}

val animals = List(Cat("Lion"), Dog("Fido"), Cat("Bob"))
println(detFirst(animals))


def apply(g: List[Animal] => Animal)(lst: List[Animal]): Animal = {
  g(lst)
}



trait BoolExpr {
  def getVars: List[String]
  def eval(store: Map[String, Boolean]): Boolean
}


case object True extends BoolExpr {
  override def getVars: List[String] = List.empty

  override def eval(store: Map[String, Boolean]): Boolean = true
}

case object False extends BoolExpr {
  override def getVars: List[String] = List.empty

  override def eval(store: Map[String, Boolean]): Boolean = false
}
case class Symbol(s: String) extends BoolExpr {
  override def getVars: List[String] = List(s)

  override def eval(store: Map[String, Boolean]): Boolean = {
    store.getOrElse(s, false)
  }
}

case class And(e1: BoolExpr, e2: BoolExpr) extends BoolExpr {
  override def getVars: List[String] =
    e1.getVars ++ e2.getVars

  override def eval(store: Map[String, Boolean]): Boolean =
    e1.eval(store) && e2.eval(store)
}


case class Or(e1: BoolExpr, e2: BoolExpr) extends BoolExpr {
  override def getVars: List[String] =
    e1.getVars ++ e2.getVars

  override def eval(store: Map[String, Boolean]): Boolean =
    e1.eval(store) || e2.eval(store)

}

case class Not(e: BoolExpr) extends BoolExpr {
  override def getVars: List[String] = e.getVars

  override def eval(store: Map[String, Boolean]): Boolean = !e.eval(store)
}





implicit def boolToExpr(b: Boolean): BoolExpr = if (b) True else False


extension (e: BoolExpr) {
  def &&(other: BoolExpr): BoolExpr = And(e, other)
  def ||(other: BoolExpr): BoolExpr = Or(e, other)
  def unary_! : BoolExpr = Not(e)
}

val expr = Symbol("x") && Symbol("y") || !Symbol("z")

class Solver(formula: BoolExpr) {
  type Store = Map[String,Boolean]

  def interpretations: List[Store] = {
    def generate(store: Store, vars: List[String]): List[Store] = vars match {
      case Nil => List(store)
      case v :: rest =>
        generate(store + (v -> true), rest) ++
          generate(store + (v -> false), rest)
    }
    generate(Map.empty, formula.getVars.distinct)
  }

  def solve: Option[Store] = {
    interpretations.find(formula.eval)
  }
}



