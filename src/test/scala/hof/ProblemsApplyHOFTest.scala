package hof

import list.implementation.SinglyLinkedIntList
import org.scalatest._
//import assignment1.ProblemsApplyHOF

class ProblemsApplyHOFTest extends FunSuite {

  test("testSumOddNumbers") {
    assert(ProblemsApplyHOF.sumOddNumbers(SinglyLinkedIntList(1, 2, 3, 4, 5)) === 9)
  }

  test("testCountEvenNumbers") {
    assert(ProblemsApplyHOF.countEvenNumbers(SinglyLinkedIntList(1, 2, 3, 4, 5)) === 2)
  }

  test("testMultiplyAndFilterEven") {
    assert(ProblemsApplyHOF.multiplyAndFilterEven(SinglyLinkedIntList(1, 2, 3), 3) === SinglyLinkedIntList(6))
  }

  test("testFindMin") {
    assert(ProblemsApplyHOF.findMin(SinglyLinkedIntList(3, 5, 1, 2, 3, 4, 5)) === 1)
  }
}
