package recfun

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange

  test("countChange: example given in instructions") {
    assert(countChange(4,List(1,2)) === 3)
  }

//  test("countChange: sorted 40 CHF: 5") {
//    printf("--------------\n")
//    printf("\nsorted 40:\n")
//    assert(countChange(40, List(5)) === 1)
//  }
//
//  test("countChange: sorted 40 CHF: 5, 10") {
//    assert(countChange(40, List(5, 10)) === 5)
//  }
//
//  test("countChange: sorted 40 CHF: 5, 10 20") {
//    assert(countChange(40, List(5, 10, 20)) === 9)
//  }
//
//  test("countChange: sorted 50 CHF: 5, 10 20") {
//    assert(countChange(50, List(5, 10, 20)) === 12)
//  }

//  test("countChange: sorted 80 CHF: 5, 10") {
//    assert(countChange(80, List(5, 10)) === 9)
//  }

//  test("countChange: sorted 100 CHF: 5, 10") {
//    assert(countChange(100, List(5, 10)) === 11)
//  }
//
//  test("countChange: sorted 100 CHF: 5, 10 20") {
//    assert(countChange(100, List(5, 10, 20)) === 36)
//  }

      test("countChange: sorted CHF") {
      assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
    }

  test("countChange: no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }

}
