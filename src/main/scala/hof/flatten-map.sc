def flatten(l: List[Any]): List[Any] = l match {
  case Nil => l // or Nill, its the same
  case (head: List[_]) :: tail => flatten(head) ++ flatten(tail)
  case x :: xs => x :: flatten(xs)
}

flatten(List(List(1, List(2), 4, List(), List(5))))

def flatMap[A, B](l: List[Any], f: Any => B): List[Any] = l match {
  case Nil => l
  case (head: List[_]) :: tail => head ++ flatMap(tail, f)
  case x :: xs => f(x) :: flatMap(xs, f)
}

//CANT TEST THAT SHEEEET AGAIN:
//flatMap(List(1,2,3),X=>List(X)++(1 to X))

flatMap(List(1, 2, 3), X => List(X) ++ (1 to 3))
flatMap(List(1, 2, 3), X => List(X, (1 to 3).toList))