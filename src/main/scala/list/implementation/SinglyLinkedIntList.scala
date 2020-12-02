package list.implementation

import list.traits.IntList

import scala.annotation.tailrec

/**
 * A companion object for the singly linked list.
 * This enables creating lists list this: val list = SinglyLinkedIntList(1,2,3)
 * which results in Cons(1,Cons(2,Cons(3,Empty))))
 */
object SinglyLinkedIntList {


  /** The apply function is a special function in scala.
   * It can be invoked with SinglyLinkedIntList.apply(args) or simply SinglyLinkedIntList(args).
   * This particular implementation of it is also a variadic function, i.e.
   * a function which accepts one or more arguments of the same type (integers) as parameters.
   */
  //inside this method xs is of type Seq[int]
  def apply(xs: Int*): SinglyLinkedIntList = xs match {
    case Seq() => Empty
    //: _* results in the sequence being passed as multiple parameters - (1,2,3) instead of Seq[Int]{1,2,3}
    case _ => Cons(xs.head, SinglyLinkedIntList(xs.tail: _*))
  }
}

abstract class SinglyLinkedIntList extends IntList {

  override def prefix(other: IntList): IntList =
    if (other.isEmpty) this else Cons(other.head, this.prefix(other.tail))

  override def size: Int = {
    @tailrec
    def sizeH(size: Int, list: IntList): Int = {
      list match {
        case Empty => size
        case Cons(_, tail) => sizeH(size + 1, tail)
      }
    }

    sizeH(0, this)
  }

  override def map(mapFunc: Int => Int): IntList = this match {
    case Empty => this
    case Cons(head, tail) => Cons(mapFunc(head), tail.map(mapFunc))
  }

  override def filter(filterFunc: Int => Boolean): IntList = this match {
    case Empty => this
    case Cons(head, tail) =>
      if (filterFunc(head)) {
        Cons(head, tail.filter(filterFunc))
      } else tail.filter(filterFunc)
  }

  // von links nach rechts mit Helfer def
  def foldLeft2(initial: Int)(reduceFunc: (Int, Int) => Int): Int = {
    @tailrec
    def foldLeftH(list: IntList, init: Int, reduceFunc: (Int, Int) => Int): Int = list match {
      case Empty => init
      //case Cons(head, tail) => reduceFunc(head, foldLeftH(tail, initial, reduceFunc))
      case Cons(head, tail) => foldLeftH(tail, reduceFunc(init, head), reduceFunc)
    }

    foldLeftH(this, initial, reduceFunc)
  }


  override def forAll(predicateFunc: Int => Boolean): Boolean = this match {
    case Empty => true
    case Cons(head, tail) => if (predicateFunc(head)) tail.forAll(predicateFunc)
    else false
  }


  // von links nach rechts ohne Helfer def
  override def foldLeft(initial: Int)(reduceFunc: (Int, Int) => Int): Int = {
    this match {
      case Empty => initial
      //falschrum case Cons(head, tail) => reduceFunc(head, foldLeftH(tail, initial, reduceFunc))
      case Cons(head, tail) => tail.foldLeft(reduceFunc(initial, head))(reduceFunc)
    }
  }

  override def reduceLeft(reduceFunc: (Int, Int) => Int): Int = {
    this match {
      case Cons(head, Empty) => head
      case Cons(head, tail) => reduceFunc(head, tail.reduceLeft(reduceFunc))
    }
  }

  override def foldRight(initial: Int)(reduceFunc: (Int, Int) => Int): Int = {
    this match {
      case Empty => initial
      case Cons(head, tail) => reduceFunc(head, tail.foldRight(initial)(reduceFunc))
    }
  }

  //(1, 2, 3).reduceRight(_ - _) === 2)
  override def reduceRight(reduceFunc: (Int, Int) => Int): Int =
    this match {
      case Cons(head, Empty) => head
      case Cons(head, tail) => reduceFunc(head, tail.reduceRight(reduceFunc))

    }

  override def insertionSort: IntList = {
    def insertionSortH(sortedList: IntList, oldList: IntList): IntList = oldList match {
      case Empty => sortedList
      case Cons(head, tail) => insertionSortH(sortedList.insertSorted(head), tail)
    }

    insertionSortH(SinglyLinkedIntList(), this)
  }

  def insertionSortPhil: IntList = {
    @tailrec
    def insertIntoList(i: Int, listing: IntList): IntList = {
      if (i < this.size) insertIntoList(i + 1, listing.insertSorted(this.get(i)))
      else listing
    }

    insertIntoList(0, SinglyLinkedIntList())
  }

  override def insertSorted(elem: Int): IntList = {
    this match {
      case Empty => Cons(elem, Empty)
      case Cons(head, tail) => if (head > elem) Cons(elem, this) else Cons(head, tail.insertSorted(elem))

    }
  }

  override def foldLeft[A](initial: A)(reduceFunc: (A, Int) => A): A = this match {
    case Empty => initial
    //falschrum case Cons(head, tail) => reduceFunc(head, foldLeftH(tail, initial, reduceFunc))
    case Cons(head, tail) => tail.foldLeft(reduceFunc(initial, head))(reduceFunc)
  }

  override def flip: IntList = this match {
    case Empty => Empty
    case Cons(head, tail) => Cons(head, Empty).prefix(tail.flip)
  }

}