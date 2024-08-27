import scala.annotation.tailrec


// a se deschide in InelliJ din `HW1-Functions-as-sets`

object FSets{

  import scala.annotation.tailrec


  type Set = Int => Boolean

  def profileID: Int = 111226

  /**
   * setul vid este setul pentru care expresia este evaluata la false
   * pentru orice elment
   *
   * @return  un set cu niciun element
   */
  def emptySet(): Set = {
    _ => false
  }



  def lengthSet(s: Set): Int = {

    @tailrec
    def helper(crt: Int, len: Int): Int = {
      if (crt > Int.MaxValue) len
      else if (s(crt)) helper(crt + 1, len + 1)
      else helper(crt + 1, len)
    }

    helper(Int.MinValue, 0)
  }

  /**
   * 1.
   * @param x   valoarea din set
   * @return    setul, o expresie lambda (functie anonima)
   */
  def singleton(x: Int): Set = {
    (el: Int) => (el == x)
  }


  /**
   * 2.
   *
   * @param e   valoarea pentru care verificam apartenenta la set
   * @param s   setul
   * @return    `true`, daca elementul se gaseste sau nu in set
   *            `false`, daca elementul nu se gaseste in set
   */
  def member(e: Int)(s: Set): Boolean = {
    s(e) == true
  }

  /**
   * ex2
   * @param x   un element nou din set
   * @param s   setul deja existent
   *            o expresie lambda (functie anonima)
   *            formata dintr-o succesiune de `SAU logice`
   * @return    setul nou
   *            expresia lambda, la care adaugam `_ == x`
   */
  def ins(x: Int)(s: Set): Set = {
    (el: Int) => (el == x || s(x))
  }

  /**
   * ex3
   * @param start
   * @param stop
   * @return
   */
  def fromBounds(start: Int, stop: Int): Set = {

    @tailrec
    def helper(start: Int, stop: Int, setacc: Set): Set = {
      if (start > stop) setacc
      else helper(start + 1, stop, ins(start)(setacc))
    }


    helper(start, stop, emptySet())
  }


  /**
   * ex4
   * @param s1
   * @param s2
   * @return
   */
  def union (s1: Set, s2: Set): Set = {

    @tailrec
    def helper(crt: Int, acc: Set): Set = {
      if (crt > Int.MaxValue) acc
      else {
        if (s1(crt) || s2(crt))
          helper(crt + 1, ins(crt)(acc))
        else
          helper(crt + 1, acc)
      }
    }


    helper(Int.MinValue, emptySet())
  }


  /**
   * ex6
   * @param s1
   * @return
   */
  def complement(s1: Set): Set = {

    @tailrec
    def helper(crt: Int, acc: Set): Set = {
      if (crt > Int.MaxValue) acc
      else {
        if (s1(crt)) helper(crt + 1, acc)
        else helper(crt + 1, ins(crt)(acc))
      }
    }

    helper(Int.MinValue, emptySet())
  }


  /**
   * ex7
   * @param b
   * @param start
   * @param stop
   * @param s
   * @return
   */
  def sumSet(b: Int)(start: Int, stop: Int)(s: Set): Int = {

    @tailrec
    def auxSum(crt: Int, acc: Int): Int = {
      if (crt < start || crt > stop) acc
      else {
        if (s(b) == true) auxSum(crt + 1, acc + crt)
        else auxSum(crt + 1, acc)
      }
    }


    auxSum(start, b)
  }

  def foldLeftSet(b:Int)(op: (Int,Int) => Int)(start: Int, stop: Int)(s: Set): Int = {
    def aux(crt: Int, acc: Int): Int = ???
    ???
  }

  /**
   * ex10
   * @param b
   * @param op
   * @param start
   * @param stop
   * @param s
   * @return
   */
  def foldRightSet(b:Int)(op: (Int,Int) => Int)(start: Int, stop: Int)(s: Set): Int = ???

  /**
   * ex10
   * @param p
   * @param s
   * @return
   */
  def filter(p: Int => Boolean)(s: Set): Set = {

    @tailrec
    def helper(crt: Int, acc: Set): Set = {
      if (crt > Int.MaxValue) acc
      else {
        if (s(crt) && p(crt))
          helper(crt + 1, ins(crt)(s))
        else
          helper(crt + 1, s)
      }
    }

    helper(Int.MinValue, emptySet())
  }

  def partition(p: Int => Boolean)(s: Set): (Set,Set) = {

    @tailrec
    def helper(crt: Int)(leftHand: Set, rightHand: Set): (Set, Set)= {
      if (crt > Int.MaxValue) (leftHand, rightHand)
      else {
        (s(crt) == true, p(crt) == true) match {
          case (false, _) => helper(crt + 1)(leftHand, rightHand)
          case (true, true) => helper(crt + 1)(ins(crt)(leftHand), rightHand)
          case (true, false) => helper(crt + 1)(leftHand, ins(crt)(rightHand))
        }
      }
    }


    helper(Int.MinValue)(emptySet(), emptySet())
  }

  def forall(cond: Int => Boolean)(start: Int, stop: Int)(s: Set): Boolean = ???

  def exists(cond: Int => Boolean)(start: Int, stop: Int)(s: Set): Boolean = ???

  /**
   * ex14
   * @param k
   * @return
   */
  def setOfDivByK(k: Int): Set = {
    if (k == 0) {
      println("Illegal divison by 0")
      emptySet()
    } else {
      val allInts = fromBounds(Int.MinValue, Int.MaxValue)
      filter(el => el % k == 0)(allInts)
    }
  }

  /**
   * ex15
   * @param k
   * @param start
   * @param stop
   * @param s1
   * @param s2
   * @return
   */
  def moreDivs(k: Int)(start: Int, stop:Int)(s1: Set, s2: Set): Boolean = {


    val subset1 = filter(el => el % k == 0 && start <= k && k <= stop)(s1)
    val subset2 = filter(el => el % k == 0 && start <= k && k <= stop)(s2)

    val len1 = lengthSet(subset1)
    val len2 = lengthSet(subset2)

    (len1 > len2)
  }
}

