
def filter[E](I: List[E], pred: E => Boolean): List[E] = I match {
  case Nil => Nil
  case x :: xs => if (pred(x)) x :: filter(xs, pred) else filter(xs, pred)
}

val l = List(1, 4, 2, 5, 7)
filter[Int](l, x => (x % 2 == 0))

val lc = List('D', 'A', '2', 'B', 'C')
filter[Char](lc, x => (x % 2 == 0))

def map[E, G](I: List[E], pred: E => G): List[G] = I match {
  case Nil => Nil
  case x :: xs => pred(x) :: map(xs, pred)
}



val lint = List(68, 65, 50, 66, 67)
map[Int, Char](lint, x => x.asInstanceOf[Char])

//von links nach rechts
def aggregate(I: List[Int], pred: (Int, Int) => Int, base: Int): Int = I match {
  case x :: Nil => pred(base, x)
  case x :: xs => aggregate(xs, pred, pred(x, xs.head))
}

//SOLUTION von rechts nach links
def fold(I: List[Int], pred: (Int, Int) => Int, base: Int): Int = I match {
  case Nil => base
  case x :: xs => pred(x, fold(xs, pred, base))
}

//typenparameter einfügen
def foldType[E, G](I: List[E], pred: (E, G) => G, base: G): G = I match {
  case Nil => base
  case x :: xs => pred(x, foldType(xs, pred, base))
}

foldType[Int, String](l, (x, y) => x + y, "")

aggregate(l, (x, y) => x + y, 0)
aggregate(l, (x, y) => x - y, 0)
aggregate(l, (x, y) => x * y, 0)
aggregate(l, (x, y) => x % y, 0)