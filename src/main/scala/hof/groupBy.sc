import scala.collection.{immutable, mutable}


val l = List(1, 2, 3, 4, 5, 6) //L

val example = Map(2 -> List(2, 5), 1 -> List(1, 4), 0 -> List(3, 6))
//l.groupBy(_ % 3) -> 2,1,0 F


//SOLUTION
//mutable but values only
def groupByS1[U, L](list: Iterable[L])(f: L => U): mutable.Map[U, List[L]] = {
  val res = mutable.Map[U, List[L]]()
  for (x <- list) {
    val groupByValue = f(x)
    res.update(groupByValue, x :: res.getOrElse(groupByValue, List()))
  }
  res
}

// :'( just cant test my functions:
//!%§&"$§%$%U/%⅝¤⅜:
//with , not working: groupByS1(l,_%3)
//OKOKOKOK ju NEEED ()() double parameter to get this done:
groupByS1(l)(_ % 3)
l.groupBy(_ % 3)


//immutable and variable
def groupByS2[U, L](list: List[L], f: L => U): Map[U, List[L]] = {
  var res = immutable.Map[U, List[L]]()
  for (x <- list) {
    //val groupByValue = f(x)
    //res = res.updated(groupByValue,x::res.getOrElse(groupByValue,List()))
    if (res.contains(f(x))) {
      res = res.updated(f(x), List().appended(x))
    } else res = res.updated(f(x), List(x))
  }
  res //.mapValues(_.reverse)
}

groupByS1(l)(_ % 3)
l.groupBy(_ % 3)

//immutable and aggregation with foldLeft
def groupByS3[U, L](list: List[L], f: L => U): Map[U, List[L]] = {
  list.foldLeft(immutable.Map[U, List[L]]()) {
    (map, x) =>
      val groupByVal = f(x)
      map.updated(groupByVal, x :: map.getOrElse(groupByVal, List.empty))
  } //.mapValues(_.reverse)
}

groupByS1(l)(_ % 3)
l.groupBy(_ % 3)

/*
//my try:

//mutable but val
def groupBy1[L](list: List[L], f: L => L, mp: mutable.Map[L, List[L]]): mutable.Map[L, List[L]] = {
  for (x <- list) {
    if (mp.contains(f(x))) {
      mp.update(f(x), List().appended(x))
    } else mp.update(f(x), List(x))
  }
  mp
}



//immutable and var
def groupBy2[L](list: List[L], f: L=>L ): immutable.Map[L, List[L]] = {
  var mp = immutable.Map()
  for (x <- list) {
    if (mp.contains(f(x))) {

      mp = mp++Map(f(x)->List().appended(x))
    } else mp = mp++Map(f(x)->List(x))
  }
  mp
}


//foldLeft aggregation
def groupBy3[L](list: List[L], f: L => L, mp: immutable.Map[L, List[L]]): immutable.Map[L, List[L]] = {
  for (x <- list) {
    if (mp.contains(f(x))) {
      mp.updated(f(x), List().appended(x))
    } else mp.updated(f(x), List(x))
  }
  mp
}

val map = mutable.Map('a'->List())
groupBy1(l, _ % 3, map)

var map2 = immutable.Map('a'->List())
groupBy2(l, _ % 3, map2)

l.groupBy(_ % 3)
*/

/*
*
}
val m = Map()
list
m.get(f) match {
  case None =>
    for (x <- list) {
      if (f = x)
        list.add(x)
    }
    m.add(f, list)
  case Some(x) => for (x <- list) {
    if (f = x)
      list.add(x)
  }
    m.add(f, list)
}
* */



