package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if ((c == 0) || (c == r)) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      val filteredChars = chars.filter(c => (c == '(') || (c == ')'))
      if (filteredChars.isEmpty || (filteredChars == "()".toList)) true
      else {
//        printf("balance: %s -> %s\n", chars, filteredChars)
        val rightParenPos = filteredChars.indexOf(')')
        if (rightParenPos <= 0) false // rightParenPos == -1 => left  paren present but no right paren
                                      // rightParenPos == 0  => right paren present but no left  paren before it
        else {
//          printf("balance: %s -> %s\n", chars, filteredChars)
//          printf("balance: rightParenPos: %d\n", rightParenPos)
//          false
//            balance(filteredChars.take(rightParenPos * 2)) &&
//            balance(filteredChars.drop(rightParenPos * 2))
          val simplifiedChars = filteredChars diff "()".toList // shd remove first occurence of '()' only
//          printf("balance: %s -> %s\n", filteredChars, simplifiedChars)
          balance(simplifiedChars)
        }
      }
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      val sortedCoins = coins.sortWith(_ > _)

      def countCombinations(acc: Int, thsCoinsHead: Int): Int = {
        if (thsCoinsHead < 0) acc
        else {
          val changeRemainder = money - (sortedCoins.head * thsCoinsHead)
          assert(changeRemainder >= 0)

          if (changeRemainder == 0) countCombinations(acc + 1, thsCoinsHead - 1)
          else {
            //            printf("countChange:   thsCoinsHead: %d; changeRemainder: %d; coinsRemainder: %s\n",
            //              thsCoinsHead, changeRemainder, coinsRemainder)
            countCombinations(acc + countChange(changeRemainder, sortedCoins.tail), thsCoinsHead - 1)
          }
        }
      }

//      printf("countChange: money: %d; sortedCoins: %s\n", money, sortedCoins)
      if ((money <= 0) || coins.isEmpty) 0
      else if (coins.length == 1) {
        if (money % coins.head == 0) {
//          printf("countChange:   money: %d; sortedCoins: %s; yes: %d\n",
//            money, sortedCoins, 1)
          1
        } else 0
      }
      else {
        // Find out how many other combinations of coins.head * n will yield correct change
        countCombinations(0, money / sortedCoins.head)
      }
    }
  }
