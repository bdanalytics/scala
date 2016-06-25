package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s11 = singletonSet(11)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2), "Singleton 1 should not contain 2")
//      assert(contains(s1, 2), "Singleton 1 should not contain 2")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection contains elements in both sets") {
    new TestSets {
      val s = intersect(union(s1, s2), s1)
      assert( contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  test("diff contains elements in one set but not in other set") {
    new TestSets {
      val s = diff(s2, s1)
      assert(!contains(s, 1), "Diff 1")
      assert( contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
    }
  }

  test("filter contains elements in one set and satisfying a predicate") {
    new TestSets {
      val s = filter(union(s1, s11), (_ >= 10))
      assert(!contains(s,  1), "Filter  1")
      assert(!contains(s,  2), "Filter  2")
      assert( contains(s, 11), "Filter 11")
    }
  }

  test("forall checks if all elements in the set satisfy a specified predicate") {
    new TestSets {
      val s = union(s1, s11)
//      printSet(s)
      assert(!forall(s,  (_ < 10 )), "Filter  <  10")
      assert( forall(s,  (_ < 100)), "Filter  < 100")
    }
  }

  test("exists checks if at least one element in the set satisfy a specified predicate") {
    new TestSets {
      val s = union(s1, s11)
      //      printSet(s)
      assert( exists(s,  (_ < 10 )), "exists  <  10")
      assert(!exists(s,  (_ > 100)), "exists  > 100")
    }
  }

  test("map creates a new set by applying a specified function to all elements of a set") {
    new TestSets {
      val s = union(s1, s11)
      //      printSet(s)
      assert( contains(map(s, (x) => x),  1), "id map s1 ")
      assert(!contains(map(s, (x) => x),  2), "id map s2 ")
      assert( contains(map(s, (x) => x), 11), "id map s11")

      assert( contains(map(s, (x) => x * x),   1), "sq map s1 ")
      assert(!contains(map(s, (x) => x * x),  11), "sq map s2 ")
      assert( contains(map(s, (x) => x * x), 121), "sq map s11")
    }
  }

}
