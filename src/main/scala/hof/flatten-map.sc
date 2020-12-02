def flatten(l: List[Any]): List[Any] = l match {
  case Nil => l // or Nill, its the same
  case (head: List[_]) :: tail => flatten(head) ++ flatten(tail)
  case x :: xs => x :: flatten(xs)
}

flatten(List(List(1, List(2), 4, List(), List(5))))

def flatMap[A, B](l: List[A])(f: A => List[B]): List[Any] = l match {
  case x :: xs => f(x) :: flatMap(xs)(f)
  case _ => Nil
}

//CANT TEST THAT SHEEEET AGAIN: --> after switch , to )( it works!"!"!
flatMap(List(1, 2, 3))(X => List(X) ++ (1 to X))
flatMap(List(1, 2, 3))(X => List(X, (1 to X).toList))
