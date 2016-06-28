package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("mostRetweeted: empty set1") {
    new TestSets {
//      println("set1: " + set1)
      try {
        set1.mostRetweeted
      } catch {
        case e: NoSuchElementException => assert(true)
      }
    }
  }

  test("mostRetweeted: set5") {
    new TestSets {
//      println("set5: " + set5)
      val set5maxRetweeted = set5.mostRetweeted
      assert(set5maxRetweeted.text == "a body" ||
             set5maxRetweeted.text == "b body" )
    }
  }

  test("descending: set5") {
    new TestSets {
      println("set5: " + set5)
      val trends = set5.descendingByRetweet
      println("trends: " + trends)
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

//  test("AndroidGalaxy TweetSet") {
//    val setSz = size(GoogleVsApple.AndroidGalaxyTweets)
//    println("AndroidGalaxy TweetSet: size: " + setSz)
//    if (setSz > 0)
//      println("AndroidGalaxy TweetSet: mostRetweeted: " + GoogleVsApple.AndroidGalaxyTweets.mostRetweeted)
//  }
  test("Google TweetSet") {
    val setSz = size(GoogleVsApple.googleTweets)
    println("Google TweetSet: size: " + setSz)
    if (setSz > 0)
      println("Google TweetSet: mostRetweeted: " + GoogleVsApple.googleTweets.mostRetweeted)
  }

  test("Apple TweetSet") {
    val setSz = size(GoogleVsApple.appleTweets)
    println("Apple TweetSet: size: " + setSz)
    if (setSz > 0)
      println("Apple TweetSet: mostRetweeted: " + GoogleVsApple.appleTweets.mostRetweeted)
  }

  test("Trending TweetSet") {
    GoogleVsApple.trending foreach println
  }
}
