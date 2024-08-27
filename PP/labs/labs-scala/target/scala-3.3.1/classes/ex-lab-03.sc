import annotation.tailrec

// 3.1 Common list operations

// 3.1.1
def atLeastk(k: Int, l: List[Int]): Boolean = {
  if (k == 0) true
  else l match
    case Nil => false
    case _ :: xs => atLeastk(k - 1, xs)
}

// 3.1.1
def atLeastkPred(pred: Int => Boolean)(k: Int, l: List[Int]): Boolean = {
  if (k == 0) true
  else l match
    case Nil => false
    case x :: xs =>
      if (pred(x) == true) atLeastkPred(pred)(k - 1, xs)
      else atLeastkPred(pred)(k, xs)
}



// 3.1.2
//take(3,List(1,2,3,4,5)) = List(1,2,3)
def take(n: Int, l: List[Int]): List[Int] = {
  if (n == 0) Nil           // List.empty[Int]
  else l match {
    case Nil => Nil         // List.empty[Int]
    case x :: xs => x +: take(n - 1, xs)
  }
}



// 3.1.3
//drop(3,List(1,2,3,4,5)) = List(4,5)
def drop(n: Int, l: List[Int]): List[Int] = {
    if (n <= 0) l
    else l match {
        case Nil => Nil     // List.empty[Int]
        case x :: xs => drop(n - 1, xs)
    }
}


// 3.1.4
//takeP(_%2 == 0)(List(1,2,3,4,5,6)) = List(2,4,6)
def takeP(p: Int => Boolean)(l: List[Int]): List[Int] = {
    l match {
        case Nil => Nil

        case x :: xs =>
            if (p(x) == true) x +: takeP(p)(l)
            else takeP(p)(l)
    }
}

// 3.1.4
//takeP(_%2 == 0)(List(1,2,3,4,5,6)) = List(2,4,6)
def takeP_tail_rec(p: Int => Boolean)(l: List[Int]): List[Int] = {

    @tailrec
    def helper(L: List[Int], acc: List[Int]): List[Int] = {
        L match {
            case Nil => acc
            case x :: xs =>

                /** operatorul `+:` adauga un element la inceputul unei liste
                 * `el +: list` */
                if (p(x) == true) helper(xs, x +: acc)
                else helper(xs, acc)
        }
    }

    helper(l, Nil)
}



// 3.1.5
// part(_%2 == 0)(List(1,2,3,4,5,6)) = (List(2,4,6),List(1,3,5))
def part(p: Int => Boolean)(l: List[Int]): (List[Int], List[Int]) = {

    @tailrec
    def helper(L: List[Int], acc1: List[Int], acc2: List[Int]): (List[Int], List[Int]) = {
        L match
            case Nil => (Nil, Nil)
            case x :: xs =>
                /** operatorul `+:` adauga un element la inceputul unei liste
                 * `el +: List[]` */
                if (p(x) == true) helper(xs, x +: acc1, acc2)
                else helper(xs, acc1, x +: acc2)
    }

    helper(l, Nil, Nil)
}



// 3.1.6
// rev(List(1,2,3,4,5,6)) = List(6,5,4,3,2,1)
def rev_with_fold_left(l: List[Int]): List[Int] = {

    @tailrec
    def helper(L: List[Int], acc: List[Int]): List[Int] = {
        L match
            case Nil => acc


            /** operatorul `+:` adauga un element la inceputul unei liste
             * `el +: List[]` */
            case x :: xs => helper(xs, x +: acc)
    }

    helper(l, Nil)
}



// 3.1.6
// rev(List(1,2,3,4,5,6)) = List(6,5,4,3,2,1)
def rev_with_fold_right(l: List[Int]): List[Int] = {
    l match
        case Nil => Nil
        /** operatorul `:+` adauga un element la sfarsitul unei liste
         *  `List[] :+ el` */
        case x :: xs => rev_with_fold_right(xs) :+ x
}


// 3.1.6
// rev(List(1,2,3,4,5,6)) = List(6,5,4,3,2,1)
def rev_foldl(l: List[Int]): List[Int] = {
    l.foldLeft(List[Int]())((acc, elem) => elem +: acc)
}


// 3.1.6
// rev(List(1,2,3,4,5,6)) = List(6,5,4,3,2,1)
def rev_foldr(l: List[Int]): List[Int] = {
    l.foldRight(List[Int]())((elem, acc) => acc :+ elem)
}








// 3.2 String processing
type Str = List[Char]

@tailrec
def printStr(l: Str): Unit = {
    l match
        case Nil => println()
        case x :: xs =>
            print(x)
            printStr(xs)
}

@tailrec
def printStrsList(l: List[Str]): Unit = {
    l match
        case Nil => println()
        case x :: xs =>
            printStr(x)
            printStrsList(xs)

}


val emailsList: List[Str] = List(
    "nununu@gmail.com",
    "mihai@gmail.com",
    "tEst@mail.com",
    "email@email.com",
    "short@ax.ro",
    ).map(x => x.toList)



// 3.2.1
def remUpper(list: List[Str]): List[Str] = {
    list.map(email => email.filterNot(c => c.isUpper))
}



// 3.2.2
def longer_v1(k: Int, list: List[Str]): List[Str] = {
    list.filterNot(email => email.length > k)
}


// 3.2.2
def longer_v2(k: Int, list: List[Str]): List[Str] = {
    list.filter(email => email.length <= k)
}


// 3.2.3
def howMany(k: Int)(list: List[Str]): Int = {
    list.foldRight(0)((str, count) => if (str.length > k) count + 1 else count)
}



// 3.2.4
def mySplit(l: Str, sep: Char): List[Str] = {
    l.foldRight(List(List[Char]()))((char, acc) =>
        if (char == sep) List() :: acc
        else (char :: acc.head) :: acc.tail
    ).filter(_.nonEmpty)


}



// 3.2.5
// `tEst@mail.com` -> `gmail`
def getDomainHelper(email: Str, hasReachedArround: Boolean, acc: Str): Str = {
    email match {
        case Nil => acc
        case chr :: str =>
            chr match
                case '@' => getDomainHelper(str, true, Nil)
                case '.' =>
                    if (hasReachedArround == true) acc
                    else getDomainHelper(str, false, acc)
                case _ =>
                    if (hasReachedArround == true) getDomainHelper(str, true, acc :+ chr)
                    else getDomainHelper(str, false, Nil)
    }
}

// 3.2.5
// `tEst@mail.com` -> `gmail`
def getEmailDomain(email: Str): Str = {
    getDomainHelper(email, false, Nil)
}

// 3.2.5
// `[tEst@mail.com, eu@yahoo.ro]` -> `[gmail, yahoo]`
def domains(emails: List[Str]): List[Str] = {
    // emails.map(email => getEmailDomain(email))
    emails.map(getEmailDomain)
}


println(take(5, List(1, 2, 3, 4)))


// 3.3 Gradebooks
val l = List(1,2,3,4,5,6,7,8,9)
l.take(3)
l.drop(3)
l.partition(_%2 == 0)


val gradebook: List[(Str, Int)] = List((List('G'),3), (List('F'), 10), (List('M'),6), (List('P'),4))

type Gradebook = List[(Str,Int)]

def printGradebook(students: Gradebook): Unit = {
    students.map(st =>
        val name = st._1
        val grade = st._2

        print("\"")
        name.map(ch => print(ch))
        print("\": " + grade)
        println()
    )
}



val students: Gradebook = List(
    ("George".toList, 9),
    ("Vasile".toList, 2),
    ("Gigi".toList, 6),
    ("Matei".toList, 10),
    ("Marcel".toList, 5),
    ("Ionel".toList, 4),
    ("Ionica".toList, 3),
    ("Gica".toList, 7)
)







// 3.3.1
def increment(g: Gradebook, p: (Str, Int) => Boolean): Gradebook =
    g.map(st =>
        val name: Str = st._1
        val grade: Int = st._2

        if (p(name, grade)) (name, grade + 1)
        else (name, grade)
    )


// 3.3.2 = average grade, must use foldRight
def average(g: Gradebook): Double = {
    val sum: Int = g.foldRight(0)((st, acc) => acc + st._2)
    val len: Int = g.length

    println("Sum = " + sum + "; Len = " + len + "\n")


    (sum.toDouble / len.toDouble)
}


// 3.3.3 = Use filter and map from Scala.
def pass(g: Gradebook): List[Str] = {
    // filtreaza notele
    // maparea extrage numele ramase
    g.filter(st => st._2 >= 5).map(st => st._1)
}


// 3.3.4 - merge-sort in ascending order
def mergeSort(l: Gradebook): Gradebook = {
    def merge(u: Gradebook, v: Gradebook): Gradebook = (u, v) match {
        case (Nil, _) => v
        case (_, Nil) => u
        case (h1 :: t1, h2 :: t2) =>
            if (h1._2 <= h2._2) h1 :: merge(t1, v)
            else h2 :: merge(u, t2)
    }
    
    val len = l.length
    if (len / 2 == 0) l
    else {
        val (left, right) = l.splitAt(len / 2)
        merge(mergeSort(left), mergeSort(right))
    }
}


// 3.3.5
def honorsList_v1(g: Gradebook): List[Str] = {
    g
      // (name: Str, grade: Int) => ...
      .filter((name, grade) => grade >= 5)
      .sortBy((name, grade) => grade)(Ordering[Int].reverse)
      .map((name, grade) => name)
}

// 3.3.5
def honorsList_v2(g: Gradebook): List[Str] = {
    g
      // (name: Str, grade: Int) => ...
      .filter((name, grade) => grade >= 5)
      .sortBy((name, grade) => -grade)      // sortam notele, shimband semnul de la + la -
      .map((name, grade) => name)
}

// 3.3.5
def honorsList_v3(g: Gradebook): List[Str] = {
    g.filter { case (_, grade) => grade >= 5 }.sortBy(-_._2).map(_._1)
}

    /*  TESTARE     */

// 3.1.6
println(rev_with_fold_right(List(1,2,3,4,5,6)))
println(rev_with_fold_left(List(1,2,3,4,5,6)))
println(rev_foldr(List(1,2,3,4,5,6)))
println(rev_foldl(List(1,2,3,4,5,6)))

// 3.2.1
printStrsList(remUpper(emailsList))

// 3.2.2
printStrsList(longer_v1(15, emailsList))
printStrsList(longer_v2(15, emailsList))


// 3.2.3
println(howMany(20)(emailsList))
println(howMany(15)(emailsList))
println(howMany(10)(emailsList))
println(howMany(5)(emailsList))


// 3.2.4
printStrsList(mySplit("bgd@gmail.com".toList, '@'))
printStrsList(mySplit("web.bgd.24@gmail.com".toList, '.'))


// 3.2.5
printStrsList(domains(emailsList))


// 3.3
printGradebook(students)

// 3.3.1

// functie:

// expresie lambda:
val lambda_cond = (name: Str, grade: Int) => grade >= 5


printGradebook(increment(students, lambda_cond))

println(average(students))


printStrsList(pass(students))

printStrsList(honorsList_v1(students))

printStrsList(honorsList_v2(students))

printStrsList(honorsList_v3(students))
