// Lab 6. For expressions

// 6.1 A small string DSL

// 6.1.1: Operatorii `<<` si `>>` shifteaza un string la stanga / dreapta
implicit class StringDSL(str: String) {

  // sterge primele `pos` caractere
  def <<(pos: Int): String = str.drop(pos)

  // sterge ultimele `pos` caractere
  def >>(pos: Int): String = str.dropRight(pos)

  // permutare la stanga
  def <<<(pos: Int): String = {
    val end = str.take(pos)
    val begin = str.drop(pos)
    begin ++ end
  }

  // permutare la dreapta
  def >>>(pos: Int): String = {
    val begin = str.takeRight(pos)
    val end = str.dropRight(pos)
    begin ++ end
  }


  def unary_~ = {
    str.map(c => if ('A' <= c && c <= 'Z') c.toLower else c.toUpper)
  }

  // echivalentul functiei `strcmp` din C
  def <=>(other: String): Int = {
    if (str > other) 1
    else if (str < other) -1
    else /* if (str == other) */ 0
  }


  // imparte un sir in calupuri de lungime data
  def /(size: Int): List[String] = {
    str.grouped(size).toList
  }
}





/**  ENCODINGS  **/
trait Player {}
case object One extends Player {
  override def toString: String = "X"
}
case object Two extends Player {
  override def toString: String = "0"
}
case object Empty extends Player {
  override def toString: String = "."
}

type Line = List[Player]
type BoardList = List[Line]

case class Board(b: BoardList) {

  // 6.2.4
  override def toString: String = {
    b.map(_.mkString("")).reduce((str1: String, str2: String) => str1 + "\n" + str2)
  }


  // varianta complicata
  def toString_v2: String = {
    def lineToString(line: Line): String = line match {
      case Nil => ""
      case chr :: str => chr.toString ++ lineToString(str)
    }

    def boardToString(board: BoardList): String = board match {
      case Nil => "\n"
      case line :: rest =>
        lineToString(line) ++ "\n" ++ boardToString(rest)
    }

    boardToString(b)
  }


  // 6.2.2
  def isFree(x: Int, y: Int): Boolean = {
    b(x)(y) match
      case One | Two => false
      case Empty => true
  }


  // 6.2.3
  def complement(p: Player): Player = {
    p match
      case Empty => Empty
      case One => One
      case Two => Two
  }

  // 6.2.5
  def getColumns: Board = {
    Board(b.transpose)
  }

  // 6.2.6
  def getFstDiag(): Line = {
    val lenSquare = b.length
    (0 until lenSquare).map(i => b(i)(i)).toList
  }

  // 6.2.6
  def getSndDiag(): Line = {
    val lenSquare = b.length
    (0 until lenSquare).map(i => b(i)(lenSquare - i - 1)).toList
  }

  // 6.2.7
  def getAboveFstDiag(): List[Line] = {
    (for (i <- 0 until b.length - 1) yield (0 to i).map(j => b(j)(i + 1 + j)).toList).toList
  }

  def getBelowFstDiag(): List[Line] = {
    (for (i <- 1 until b.length) yield (0 until b.length - i).map(j => b(i + j)(j)).toList).toList
  }

  def getAboveSndDiag(): List[Line] = {
    (for (i <- 0 until b.length - 1) yield (0 to i).map(j => b(j)(b.length - i + j - 2)).toList).toList
  }

  def getBelowSndDiag(): List[Line] = {
    (for (i <- 1 until b.length) yield (0 until b.length - i).map(j => b(i + j)(b.length - 1 - j)).toList).toList
  }

  def winner(p: Player): Boolean = {
    b.exists(line => line.forall(_ == p)) ||
      this.getColumns.b.exists(line => line.forall(_ == p)) ||
      this.getFstDiag().forall(_ == p) ||
      this.getSndDiag().forall(_ == p)
  }


  def update(p: Player)(ln: Int, col: Int): Board = {
    val newLine = b(ln).updated(col, p)
    Board(b.updated(ln, newLine))
  }
}


// 6.2.1
def makeBoard(s: String): Board = {
  def toPos(c: Char): Player = {
    c match {
      case 'X' => One
      case '0' => Two
      case _ => Empty
    }
  }

  val bdList: BoardList = s.split("\n").map(_.toList.map(toPos)).toList
  Board(bdList)
}









val board1: BoardList = List(
  List(One, Two, Empty),
  List(One, Empty, Two),
  List(One, Empty, Empty)
)


val bs = Board(board1).toString
println(Board(board1).toString_v2)


makeBoard(bs)


println("abcdefg")

println("Stergere din capatul din stanga:")
println("abcdefg" << 2)
println("abcdefg" << 3)
println("abcdefg" << 4)

println("Stergere din capatul din dreapta:")
println("abcdefg" >> 2)
println("abcdefg" >> 3)
println("abcdefg" >> 4)


println("Permutare la stanga:")
println("abcdefg" <<< 2)
println("abcdefg" <<< 3)
println("abcdefg" <<< 4)

println("Permutare la dreapta:")
println("abcdefg" >>> 2)
println("abcdefg" >>> 3)
println("abcdefg" >>> 4)


println("Schimbarea case-ului din mare in mic si vice versa:")
println(~"Xx1337ScAlAcOdErxXx")
println(~"123455")
println(~"~!@#$%^&*())[]~")
println(~"Bog Dan")


println("strcmp(`haskell`, `java`) = " + ("haskell" <=> "java"))
println("strcmp(`java`, `haskell`) = " + ("java" <=> "haskell"))
println("strcmp(`scala`, `java`) = " + ("scala" <=> "java"))
println("strcmp(`java`, `scala`) = " + ("java" <=> "scala"))
println("strcmp(`scala`, `scala`) = " + ("scala" <=> "scala"))

println("abcdefghij" / 1)
println("abcdefghij" / 2)
println("abcdefghij" / 3)
println("abcdefghij" / 4)
println("abcdefghij" / 5)
println("abcdefghij" / 6)
