implicit class StringDSL(val str: String) {

  def <<(x: Int): String = {

    if (x == 0) str
    else if (x < 0) {
      println("Invalid x " + x)
      str
    } else {
      val len = str.length
      val shift = x % len
      str.substring(shift)
    }
  }

  def >>(x: Int): String = {

    if (x == 0) str
    else if (x < 0) {
      println("Invalid x " + x)
      str
    } else {
      val len = str.length
      val shift = x % len
      str.substring(0, shift) + str.substring(shift)
    }
  }

  def -(toRemove: String): String =
    str.replaceAllLiterally(toRemove, "")

  def unary_~ = {

    def transformChar(c: Char): Char = {
      if (c.isLetter) {
        if (c.isLower) c.toUpper
        else c.toLower
      } else c
    }

    // str.map(transformChar)  // ii putem pasa doar numele functiei :)
    str.map(c => transformChar(c))
  }
}

println("odersky" << 2)
println("odersky" >> 2)
println("Buna, Bogdan!" - "B")
println("Buna, Bogdan! Buna!" - "!")
println("Buna, Bogdan! Buna!" - "Buna")
println(~"Buna, Bogdan! bunA!")




implicit class StringComparison(val str: String) {
  def <=>(other: String): Int = {
    str.compareTo(other)
  }
}


println("haskell" <=> "java")
println("scala" <=> "java")
println("scala" <=> "scala")



implicit class StringChunk(val str: String) {
  def /(chunkSize: Int): List[String] = {
    str.grouped(chunkSize).toList
  }
}


println("scalable language" / 3)






/** 6.2        Tic-Tac-Toe         **/



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
  override def toString: String = b.map(_.mkString("")).reduce((str1: String, str2: String) => str1 + "\n" + str2)
}



def makeBoard(s: String): Board = {
  def toPos(c: Char): Player =
    c match {
      case 'X' => One
      case '0' => Two
      case _ => Empty
    }

  val boardList: BoardList = s.split('\n').map(_.toList.map(toPos)).toList
  Board(boardList)
}





def isFree(b: Board, x: Int, y: Int): Boolean = {
  0 <= x && x < b.b.size && 0 <= y && y < b.b(x).size && b.b(x)(y) == Empty
}

def complement(p: Player): Player = p match {
  case One => Two
  case Two => One
  case Empty => throw new Exception("complement is not defined for Empty")
}







def getColumns(b: Board): Board = {
  Board(b.b.transpose)
}


def getFstDiag(b: Board): Line = {
  val (minSize, maxSize) = (Math.min(b.b.size, b.b.head.size), Math.max(b.b.size, b.b.head.size))
  (0 to minSize - 1).map(i => b.b(i)(i)).toList
}

def getSndDiag(b: Board): Line = {
  val (minSize, maxSize) = (Math.min(b.b.size, b.b.head.size), Math.max(b.b.size, b.b.head.size))
  (0 to minSize - 1).map(i => b.b(b.b.size - 1 - i)(i)).toList
}

def getAboveFstDiag(matrix: List[List[Int]]): List[List[Int]] = {
  (for (i <- 0 until matrix.length - 1) yield matrix(i).drop(i + 1)).toList
}

def getBelowFstDiag(matrix: List[List[Int]]): List[List[Int]] = {
  (for (i <- 1 until matrix.length) yield matrix(i).reverse.take(i)).toList
}

def getAboveSndDiag(matrix: List[List[Int]]): List[List[Int]] = {
  getBelowFstDiag(matrix.reverse).reverse
}

def getBelowSndDiag(matrix: List[List[Int]]): List[List[Int]] = {
  getAboveFstDiag(matrix.reverse).reverse
}


def winner(b: Board, p: Player): Boolean = {
  b.b.exists(line => line.forall(_ == p)) ||
    getColumns(b).b.exists(line => line.forall(_ == p)) ||
    getFstDiag(b).forall(_ == p) ||
    getSndDiag(b).forall(_ == p)
}


def update(b: Board, p: Player)(ln: Int, col: Int): Board = {
  val newLine = b.b(ln).updated(col, p)
  Board(b.b.updated(ln, newLine))
}




def next(b: Board, p: Player): List[Board] = {
  (for (i <- 0 until b.b.size; j <- 0 until b.b.head.size) yield {
    if (isFree(b, i, j)) update(b, p)(i, j) else b
  }).toList
}




val t1 =
  """X0X0X0
    |0X0X0X
    |X0X0X0
    |.XX0..
    |X00...
    |X0X0X0""".stripMargin

val t2 =
  """......
    |......
    |......
    |.XX...
    |.0000.
    |......""".stripMargin

val t3 =
  """0X0X0.
    |000.X0
    |0.0X..
    |0..0..
    |0X..0X
    |...X..""".stripMargin
