/*

de la Analiza Algoritmilor
ADT = Abstract Data Type

List = Cons(Int, Int) sau Empty
[1, 2, 3] = Cons(1, Cons(2, Cons(3, Empty)
1:2:3:[]

[] = lista Empty
1:2 -> Cons-ul


// interfata din Java
// se comporta ca `trait`-ul din Rust
trait MyList() {
  def head: Int;
  def tail: Int;
  def isEmpty: Boolean;
}

case class Empty extends MyList {
  def head:Int = ???      // `???` = este neimplementat
  def tail: MyList = ???
  def isEmpty: Boolean = true
}

case class Cons(head: Int, tail: Int) extends MyList {
  def isEmpty: Boolean = False
}





In `Scala`, avem tipul de date generic `List[T]`


def sum(a: List[Int]): Int = {
  // pattern matching
  a match {
    case Empty => 0
    case Cons(x, xs) => x + sum(xs)
  }
}


Ce este o functie de ordin superior?
R: O functie care foloseste intr-un mod sau altul alte functii
R: functii care primesc functii drep parametri
R: functii care returneaza o functie



`map`
- este o functie de ordin superior
- aplica o functie pe fiecare element dintr-o lista
list.map(f: T => T): List[T]


def toUpper(sir: List[Char]): List[Char] = {
  // expresie lambda = functie anonima
  sir.map(c => c.toUpper)
  // <=> sir.map(_ => _.toUpper)
  // <=> sir.map(toUpper)
}


list.reduce(f: (T, T) => T)
  (a, b) => a + b
  (_ + _)


list.reduce(x: T => Boolean) : (List[T], List[T])
def isEven(x: Int): Boolean = x % 2 == 0
(listTrue, listFalse) = List(1, 2, 3, 4, 5, 6).reduce(isEven)

list.foldLeft(acc: T)(f: (T, T) => T)     // primul element si acumulatorul
list.foldRight(acc: T)(f: (T, T) => T)    // ultimul element si acumulatorul
list[T]
*/


var list = List.empty[Int]




def sum(a: List[Int]): Int = {
  // pattern matching
  a match {
    case Nil => 0
    case x::xs => x + sum(xs)
  }
}


list = List(1)
println("sum(" + list + ") = " + sum(list))


list = List(1, 2, 3)
println("sum(" + list + ") = " + sum(list))

list = List(1, 2, 3 ,4, 5)
println("sum(" + list + ") = " + sum(list))


def toUpper(sir: List[Char]): List[Char] = {
  // expresie lambda = functie anonima
  sir.map(c => c.toUpper)
  // <=> sir.map(_ => _.toUpper)
}





def getListLength(l :List[Int]): Int = l.length



def atLeastk(k: Int, l: List[Int]): Boolean = {
  """
    | 3.1.1 Common list operations
    |""".stripMargin

  if (k == 0) true
  else {
    l match {
      case Nil => false
      case firstEl::restList => atLeastk(k - 1, restList)
    }
  }

}



list = List(1, 2, 3, 4, 5)

list = List(1)
println("len(" + list + ") = " + list.length)

list = List(1, 2)
println("len(" + list + ") = " + list.length)

list = List(1, 2, 3)
println("len(" + list + ") = " + list.length)

list = List(1, 2, 3, 4)
println("len(" + list + ") = " + list.length)


println(list)

println(atLeastk(3, list))
println(atLeastk(4, list))
println(atLeastk(5, list))
println(atLeastk(6, list))





def atLeastkPred(pred: Int => Boolean)(k: Int, l: List[Int]): Boolean = {
  """
    | 3.1.1
    |""".stripMargin


  l match {
    case Nil => k <= 0

    case x::xs =>
      if (pred(x) == true) atLeastkPred(pred)(k - 1, xs)
      else atLeastkPred(pred)(k, xs)
  }
}

list = List(-4, -3, -2, -1, 0, 1, 2, 3, 4)
def pred(el: Int): Boolean = el > 0


println(atLeastkPred(pred)(2, list))
println(atLeastkPred(x => x % 2 == 0)(2, list))


def take1(n: Int, l: List[Int]): List[Int] = {
  """
    | 3.1.2.
    | extrage primele n elemente
    |""".stripMargin

  if (n <= 0) Nil
  else {
    l match {
      case Nil => Nil
      case x :: xs => List(x) ::: take1(n - 1, xs) // `:::` concateneaza doua liste, respectand ordinea
    }
  }
}


def take2(n: Int, l: List[Int]): List[Int] = {
  """
    | 3.1.2.
    | extrage primele n elemente
    |""".stripMargin

  if (n <= 0) Nil
  else {
    l match {
      case Nil => List.empty[Int]
      case x :: xs => x :: take2(n - 1, xs) // `:::` concateneaza doua liste, respectand ordinea
    }
  }
}



list = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
println(take1(1, list))
println(take1(2, list))
println(take1(3, list))
println(take1(4, list))
println(take1(422, list))

list = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
println(take2(1, list))
println(take2(2, list))
println(take2(3, list))
println(take2(4, list))
println(take2(422, list))


def drop(n: Int, l: List[Int]): List[Int] = {
  """
    | 3.1.3
    | elimina primele `n` elemente
    |""".stripMargin

  if (n <= 0) l
  else l match {
    case Nil => Nil
    case x::xs => drop(n - 1, xs)
  }
}


list = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
println(drop(1, list))
println(drop(2, list))
println(drop(3, list))
println(drop(4, list))
println(drop(422, list))





def takeP(p: Int => Boolean)(l: List[Int]): List[Int] = {
  """
    | 3.1.4
    | part(_%2 == 0)(List(1,2,3,4,5,6)) = List(2,4,6)
    |""".stripMargin


  l match {
    case Nil => List.empty[Int]
    case x::xs =>
      if (p(x) == true) x :: takeP(p)(xs)
      else takeP(p)(xs)
  }
}


println(takeP(_%2 == 0)(List(1,2,3,4,5,6)))


def part(p: Int => Boolean)(l: List[Int]): (List[Int], List[Int]) = {
  """
    | 3.1.5
    | part(_%2 == 0)(List(1,2,3,4,5,6)) = (List(2,4,6),List(1,3,5))
    |""".stripMargin

  l match {
    case Nil => (List.empty[Int], List.empty[Int])
    case x :: xs => {
      var l1 = part(p)(xs)._1
      var l2 = part(p)(xs)._2
      // var (l1, l2) = part(p)(xs)

      if (p(x)) l1 = x :: l1
      else l2 = x :: l2

      (l1, l2) // the returned tuple
    }
  }
}


println(part(_%2 == 0)(List(1,2,3,4,5,6)))





def rev(l: List[Int]): List[Int] = {
  """
    | 3.1.6
    | rev(List(1,2,3,4,5,6)) = List(6,5,4,3,2,1)
    |
    | foldRight, deoarece ultimul element din lista devine primul
    | etc
    |""".stripMargin


  def helper(l: List[Int], acc: List[Int]): List[Int] = {
    l match {
      case Nil => acc
      case x :: xs => helper(xs, acc) ::: List(x)
    }
  }

  helper(l, Nil)
}

println(rev(List(1)))
println(rev(List(1, 2)))
println(rev(List(1, 2, 3)))
println(rev(List(1, 2, 3, 4)))
println(rev(List(1, 2, 3, 4, 5)))
println(rev(List(1, 2, 3, 4, 5, 6)))



type Str = List[Char]
val emails: List[Str] = List("matei@gmail.com",
  "mihai@gmail.com",
  "tEst@mail.com",
  "email@email.com",
  "short@ax.ro",
  "bogan.24@gmail.com"
).map(x => x.toList)
println(emails)



def remUpper(list: List[Str]): List[Str] = {
  """
    | 3.2.1
    | Remove uppercases from emails. (Do not use recursion). Use the Internet to find the appropriate character function.
    |
    |
    |""".stripMargin
  list.map(_.filterNot(_.isUpper))

}

println(remUpper(emails))

def longer(k: Int, list: List[Str]): List[Str] = {
  """
    | 3.2.2
    |""".stripMargin
  list.filter(_.length > k)
}

println(longer(10, emails))
println(longer(13, emails))
println(longer(15, emails))
println(longer(20, emails))



def howMany(k: Int)(list: List[Str]): Int = {
  """
    | 3.2.3
    | foldRight
    | numarul de liste pt care length >= k
    |""".stripMargin
  list.foldRight(0)((str, count) => if (str.length > k) count + 1 else count)
}


println(howMany(0)(emails))
println(howMany(1)(emails))
println(howMany(2)(emails))
println(howMany(3)(emails))
println(howMany(20)(emails))
println(howMany(30)(emails))


def mySplit(l: String, sep: Char): List[Str] = {
  """
    | 3.2.4
    | foldRight
    | delimiteaza un string in functie de un separator (strtok-ul din C)
    |""".stripMargin
  l.foldRight(List(List[Char]()))((char, acc) =>
    if (char == sep) List() :: acc
    else (char :: acc.head) :: acc.tail
  ).filter(_.nonEmpty)
}



println(mySplit("matei@gmail.com", '.'))
println(mySplit("mihai.goe@gmail.com", '.'))
println(mySplit("mihai.goe@gmail.com", '@'))




def domains(list: List[Str]): List[Str] = {
  """
    | 3.2.5
    | [text]@gmail.com
    | vrem sa extragem domeniul : `gmail`
    |""".stripMargin
  list.map(_.dropWhile(_ != '@').tail.takeWhile(_ != '.'))
}

println(domains(emails))


val l = List(1,2,3,4,5,6,7,8,9)
l.take(3)
l.drop(3)
l.partition(_%2 == 0)


val gradebook: List[(Str, Int)] = List((List('G'),3), (List('F'), 10), (List('M'),6), (List('P'),4))


type Gradebook = List[(Str,Int)] //the type Gradebook now refers to a list of pairs of String and Int


def increment(g: Gradebook, p: (Str, Int) => Boolean): Gradebook = {
  """
    | 3.3.1
    |""".stripMargin
  g.map { case (name, grade) => if (p(name, grade)) (name, grade + 1) else (name, grade) }
}

// List((List(G),3), (List(F),11), (List(M),7), (List(P),4))
println(increment(gradebook, (_, grade) => grade >= 5))


def average(g: Gradebook): Double = {
  """
    | 3.3.2
    |  Find the average grade from a gradebook. You must use foldRight
    |""".stripMargin
  val (sum, count) = g.foldRight((0, 0)) { case ((_, grade), (s, c)) => (s + grade, c + 1) }
  if (count == 0) 0 else sum.toDouble / count
}

println(average(gradebook)) // 5.75



def pass(g: Gradebook): List[Str] = {
  """
    | 3.3.2
    |""".stripMargin
  g.filter { case (_, grade) => grade >= 5 }.map { case (name, _) => name }
}

println(pass(gradebook)) // List(List(F), List(M))


def mergeSort(l: Gradebook): Gradebook = {
  """
    | 3.3.4
    | merge sort
    |""".stripMargin
  def merge(u: Gradebook, v: Gradebook): Gradebook = (u, v) match {
    case (Nil, _) => v
    case (_, Nil) => u
    case (h1 :: t1, h2 :: t2) =>
      if (h1._2 <= h2._2) h1 :: merge(t1, v)
      else h2 :: merge(u, t2)
  }

  val n = l.length / 2
  if (n == 0) l
  else {
    val (left, right) = l.splitAt(n)
    merge(mergeSort(left), mergeSort(right))
  }
}

println(mergeSort(gradebook)) // List((List(G),3), (List(P),4), (List(M),6), (List(F),10))


def honorsList(g: Gradebook): List[Str] = {
  """
    | 3.3.5
    | Write a function which takes a gradebook and reports
    | all passing students in descending order of their grade.
    |""".stripMargin
  g.filter { case (_, grade) => grade >= 5 }.sortBy(-_._2).map(_._1)
}

val gradebook: Gradebook = List(
  (List('G'), 3),
  (List('F'), 10),
  (List('M'), 6),
  (List('P'), 4)
)


println(honorsList(gradebook)) // List(List(F), List(M))
