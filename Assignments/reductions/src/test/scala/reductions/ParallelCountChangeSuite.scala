package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelCountChange._

@RunWith(classOf[JUnitRunner])
class ParallelCountChangeSuite extends FunSuite {

  test("countChange should return 0 for money < 0") {
    def check(money: Int, coins: List[Int]) = 
      assert(countChange(money, coins) == 0,
        s"countChang($money, _) should be 0")

    check(-1, List())
    check(-1, List(1, 2, 3))
    check(-Int.MinValue, List())
    check(-Int.MinValue, List(1, 2, 3))
  }

  test("countChange should return 1 when money == 0") {
    def check(coins: List[Int]) =
      assert(countChange(0, coins) == 1,
        s"countChang(0, _) should be 1")

    check(List())
    check(List(1, 2, 3))
    check(List.range(1, 100))
  }

  test("countChange should return 0 for money > 0 and coins = List()") {
    def check(money: Int) =
      assert(countChange(money, List()) == 0,
        s"countChang($money, List()) should be 0")

    check(1)
    check(Int.MaxValue)
  }

  test("countChange should work when there is only one coin") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(countChange(money, coins) == expected,
        s"countChange($money, $coins) should be $expected")

    check(1, List(1), 1)
    check(2, List(1), 1)
    check(1, List(2), 0)
    check(Int.MaxValue, List(Int.MaxValue), 1)
    check(Int.MaxValue - 1, List(Int.MaxValue), 0)
  }

  test("countChange should work for multi-coins") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(countChange(money, coins) == expected,
        s"countChange($money, $coins) should be $expected")

    check(50, List(1, 2, 5, 10), 341)
    check(250, List(1, 2, 5, 10, 20, 50), 177863)
  }


//  test("totalCoinsThreshold should return true when the number of coins is < two-thirds of the initial number of coins")
//  {
//    assert(totalCoinsThreshold(totalCoins = ))
//  }
//  test("totalCoinsThreshold should return false when the number of coins is greater than two-thirds of the initial number of coins")
//  {
//
//  }

  //  test("moneyThreshold should return true when the money is equal to two-thirds of the starting money")
  //  {
  //
  //  }
  //  test("moneyThreshold should return true when the money is < two-thirds of the starting money")
  //  {
  //
  //  }
//  test("moneyThreshold should return false when the money is greater than two-thirds of the starting money")
//  {
//
//  }

  //  test("combinedThreshold should return false when the number of coins times money greater than half of the initial number of coins times starting money")
//  {
//    assert(!combinedThreshold(100, List(1, 2)))
//  }
//  test("combinedThreshold should return true when the number of coins times money is less than or equal to half of the initial number of coins times starting money")
//  {
//
//  }

//  test("parCountChange should invoke the parallel construct 6 times for money == 16, coins == List(1) and moneyThreshold(16)")
//  {
//    assert(parCountChange(16, List(1), { case (0, List(0)) => true} ) === 1)
//  }

//  test("parCountChange with moneyThreshold should produce correct output when there are four coins and the amount is 50")
//  {
//    assert(parCountChange(50, List(1, 2, 5, 10), { case (0, List(0)) => true} ) === 341)
//  }
  //  test("parCountChange with moneyThreshold should produce correct result when there is only one coin and the amount is less than the value of the coin ")
//  {
//    assert(parCountChange(1, List(10), { case (0, List(0)) => true} ) === 0)
//  }
//  test("parCountChange with moneyThreshold should produce correct result when there is only one coin and the amount is equal to the value of the coin")
//  {
//    assert(parCountChange(1, List(1), { case (0, List(0)) => true} ) === 1)
//  }
//  test("parCountChange with moneyThreshold should produce correct output when there are two coins and the amount is 1")
//  {
//    assert(parCountChange(1, List(5, 10), { case (0, List(0)) => true} ) === 0)
//  }
//  test("parCountChange with moneyThreshold should produce correct output when there are six coins and the amount is 250")
//  {
//    assert(parCountChange(250, List(1, 2, 5, 10, 20, 50), { case (0, List(0)) => true} ) === 177863)
//  }

//  test("parCountChange with totalCoinsThreshold should produce correct result when there is only one coin and the amount is equal to the value of the coin")
//  {
//    assert(parCountChange(10, List(10), { case (0, List(0)) => true} ) === 1)
//  }
//  test("parCountChange with totalCoinsThreshold should produce correct result when there is only one coin and the amount is less than the value of the coin")
//  {
//    assert(parCountChange(1, List(10), { case (0, List(0)) => true} ) === 0)
//  }
//  test("parCountChange with totalCoinsThreshold should produce correct output when there are two coins and the amount is 1")
//  {
//    assert(parCountChange(1, List(5, 10), { case (0, List(0)) => true} ) === 0)
//  }
//  test("parCountChange with totalCoinsThreshold should produce correct output when there are four coins and the amount is 50")
//  {
//    assert(parCountChange(50, List(1, 2, 5, 10), { case (0, List(0)) => true} ) === 341)
//  }
//  test("parCountChange with totalCoinsThreshold should produce correct output when there are six coins and the amount is 250")
//  {
//    assert(parCountChange(250, List(1, 2, 5, 10, 20, 50), { case (0, List(0)) => true} ) === 177863)
//  }

//  test("parCountChange with combinedThreshold should produce correct result when there is only one coin and the amount is equal to the value of the coin ")
//  {
//    assert(parCountChange(1, List(1), { case (0, List(0)) => true} ) === 1)
//  }
//  test("parCountChange with combinedThreshold should produce correct result when there is only one coin and the amount is less than the value of the coin")
//  {
//    assert(parCountChange(1, List(10), { case (0, List(0)) => true} ) === 0)
//  }
//  test("parCountChange with combinedThreshold should produce correct output when there are two coins and the amount is 1")
//  {
//    assert(parCountChange(1, List(5, 10), { case (0, List(0)) => true} ) === 0)
//  }
//  test("parCountChange with combinedThreshold should produce correct output when there are four coins and the amount is 50")
//  {
//    assert(parCountChange(50, List(1, 2, 5, 10), { case (0, List(0)) => true} ) === 341)
//  }
//  test("parCountChange with combinedThreshold should produce correct output when there are six coins and the amount is 250")
//  {
//    assert(parCountChange(250, List(1, 2, 5, 10, 20, 50), { case (0, List(0)) => true} ) === 177863)
//  }

//  test("parCountChange should not use task construct directly and should only use the parallel construct")
//  {
//    assert(parCountChange(250, List(1, 2, 5, 10, 20, 50), { case (0, List(0)) => true} ) === 177863)
//  }

}
