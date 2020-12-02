def flattenAll(l: List[Any]): List[Any] = l match {
  case Nil => l // or Nill, its the same
  case (head: List[_]) :: tail => flattenAll(head) ++ flattenAll(tail)
  case x :: xs => x :: flattenAll(xs)
}

flattenAll(List(List(1, List(2), 4, List(), List(5))))

//flat nur eine ebene, VL solution
def flattenOne[T](l: List[List[T]]): List[T] = l match {
  case Nil => Nil // or Nill, its the same
  case (head: List[T]) :: tail => head ++ flattenOne[T](tail)
}

flattenOne(List(List(1, List(2), 4, List(), List(5))))

//flat one level and map
def flatMap[A, B](l: List[A])(f: A => List[B]): List[Any] = l match {
  case x :: xs => f(x) :: flatMap(xs)(f)
  case _ => Nil
}

//CANT TEST THAT SHEEEET AGAIN: --> after switch , to )( it works!"!"!
flatMap(List(1, 2, 3))(X => List(X) ++ (1 to X))
flatMap(List(1, 2, 3))(X => List(X, (1 to X).toList))
