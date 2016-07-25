package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite  {

  test("wordOccurrences:") {
    assert(wordOccurrences("ok") === List(('k', 1), ('o', 1)))
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("sentenceOccurrences:") {
    assert(sentenceOccurrences(List("Robert", "er")) === List(('b', 1), ('e', 2), ('o', 1), ('r', 3), ('t', 1)))
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }


  test("dictionaryByOccurrences.get:") {
//    assert(dictionaryByOccurrences.get(List(('a', 1))).map(_.toSet) === Some(Set("a")))
//    assert(dictionaryByOccurrences.get(List(('i', 1))).map(_.toSet) === Some(Set("i")))
    assert(dictionaryByOccurrences.get(List(('x', 1))).map(_.toSet) === None)
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
    assert(dictionaryByOccurrences.get(
      List(('e',1), ('i',1), ('l',2), ('r',1), ('u',1), ('x',1), ('z',1))).map(_.toSet) === None)
  }


  test("word anagrams:") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("combinations:") {
    assert(combinations(Nil) === List(Nil))
    assert(combinations(wordOccurrences("a")).toSet === List(
            List(),
            List(('a', 1))
    ).toSet)
    assert(combinations(wordOccurrences("ok")).toSet === List(
      List(),
      List(('k', 1)),
      List(('o', 1)),
      List(('k', 1), ('o', 1))
    ).toSet)
    assert(combinations(wordOccurrences("car")).toSet === List(
      List(),
      List(('c', 1)),
      List(('a', 1)),
      List(('r', 1)),
      List(('a', 1), ('c', 1)),
      List(('a', 1), ('r', 1)),
      List(('c', 1), ('r', 1)),
      List(('a', 1), ('c', 1), ('r', 1))
    ).toSet)

    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 1)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 1)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)

  }


  test("subtract:") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)

    val larder = List(('a', 1), ('d', 1), ('e', 1), ('l', 1), ('r', 2))
//    val r = List(('r', 1))
    val larde = List(('a', 1), ('d', 1), ('e', 1), ('l', 1), ('r', 1))
    assert(subtract(larder, r) === larde)

    val linuxrulez = List(('e', 1), ('i', 1), ('l', 2), ('n', 1), ('r', 1), ('u', 2), ('x', 1), ('z', 1))
    val nu = List(('n', 1), ('u', 1))
    val lixrulez = List(('e', 1), ('i', 1), ('l', 2), ('r', 1), ('u', 1), ('x', 1), ('z', 1))
    assert(subtract(linuxrulez, nu) === lixrulez)
  }

  test("sentence anagrams:") {
    assert(sentenceAnagrams(List()) === List(Nil))

    val yesman = List("Yes", "man")
    val yesmanAnm = List(
      List("as", "en", "my"),
      List("as", "my", "en"),
      List("en", "as", "my"),
      List("en", "my", "as"),
      List("my", "as", "en"),
      List("my", "en", "as"),
      List("my", "sane"),
      List("my", "Sean"),

      List("man", "yes"),
      List("men", "say"),
      List("say", "men"),
      List("yes", "man"),

      List("sane", "my"),
      List("Sean", "my")
    )
    assert(sentenceAnagrams(yesman).toSet === yesmanAnm.toSet)

    val linuxrulez = List("Linux", "rulez")
    val linuxrulezAnm = List(
      List("Rex", "Lin", "Zulu"),
      List("Rex", "nil", "Zulu"),
      List("Rex", "Zulu", "Lin"),
      List("Rex", "null", "Uzi"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),

      List("nil", "Zulu", "Rex"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Uzi", "null", "Rex"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),

      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(linuxrulez).toSet === linuxrulezAnm.toSet)
  }

}
