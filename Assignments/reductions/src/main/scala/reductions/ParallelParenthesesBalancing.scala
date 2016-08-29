package reductions

import scala.annotation._
import org.scalameter._
import common._
import scala.reflect.ClassTag

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
//    val filteredChars = chars.filter(c => (c == '(') || (c == ')'))
////    println(s"ParallelParenthesesBalancing: balance: chars.getClass: ${ClassTag(chars.getClass)}; filteredChars.getClass: ${ClassTag(filteredChars.getClass)}")
//    println(s"ParallelParenthesesBalancing: balance: chars.toList: ${chars.toList}; filteredChars.toList: ${filteredChars.toList}")
////    println("ParallelParenthesesBalancing: balance: chars: " + chars + "; filteredChars: " + filteredChars)
//    if (filteredChars.isEmpty || (filteredChars == "()".toArray)) true
//    else {
//      //        printf("balance: %s -> %s\n", chars, filteredChars)
//      val rightParenPos = filteredChars.indexOf(')')
//      if (rightParenPos <= 0) false // rightParenPos == -1 => left  paren present but no right paren
//      // rightParenPos == 0  => right paren present but no left  paren before it
//      else {
//        //          printf("balance: %s -> %s\n", chars, filteredChars)
//        //          printf("balance: rightParenPos: %d\n", rightParenPos)
//        //          false
//        //            balance(filteredChars.take(rightParenPos * 2)) &&
//        //            balance(filteredChars.drop(rightParenPos * 2))
//        val simplifiedChars = filteredChars diff "()".toList // shd remove first occurence of '()' only
//        //          printf("balance: %s -> %s\n", filteredChars, simplifiedChars)
//        balance(simplifiedChars)
//      }
//    }

    def code(ch: Char): Int = ch match {
      case '(' => 1
      case ')' => -1
      case _ => 0
    }
    @tailrec
    def loop(chLs: List[Char], acc: Int = 0): Int = chLs match {
      case head::tail if acc >= 0 => loop(tail, acc + code(head))
      case _ => acc
    }
    loop(chars.toList) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : (Int, Int) = {
      if (idx < until) {
        chars(idx) match {
          case '(' => traverse(idx + 1, until, arg1 + 1, arg2)
          case ')' =>
            if (arg1 > 0) traverse(idx + 1, until, arg1 - 1, arg2)
            else traverse(idx + 1, until, arg1, arg2 + 1)
          case _ => traverse(idx + 1, until, arg1, arg2)
        }
      } else (arg1, arg2)
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      val size = until - from
      if (size > threshold) {
//        println(s"ParallelParenthesesBalancing: reduce: from: $from; until: $until; size: $size")

        val halfSize = size / 2
        val ((a1, a2), (b1, b2)) = parallel(reduce(from, from + halfSize), reduce(from + halfSize, until))
        if (a1 > b2) {
          // )))((())(( => )))(((
          (a1 - b2 + b1) -> a2
        } else {
          // )))(()))(( => ))))((
          b1 -> (b2 - a1 + a2)
        }

//        (0, 0)
      }
      else traverse(from, until, 0, 0)
    }

    reduce(0, chars.length) equals (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
