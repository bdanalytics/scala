package forcomp
//package scala.tools.nsc.io._

import java.io._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary
//  println("dictionary.take(5): " + dictionary.take(5))
//  println("wrdLenDct.take(5): " + dictionary
//        .map(w => (w, w.length))
//        .sortWith((e1, e2) => (e1._2 < e2._2) || ((e1._2 == e2._2) && (e1._1 < e2._1)))
//        .take(5))

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase.groupBy(c => c).mapValues(_.size).toList.sorted

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences =
    s.flatMap(w => wordOccurrences(w)).groupBy(p => p._1).map(kv => (kv._1, kv._2.map(kvi => kvi._2).sum)).toList.
      sorted

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    val WordOccWordPairLst = dictionary.map(w => (wordOccurrences(w), w))
    val WordOccGroupLst = WordOccWordPairLst.groupBy({ case (o, w) => o })
    val simplifiedWordOccGroupLst = WordOccGroupLst.mapValues(_.flatMap({ case (o, w) => List(w) }))
    simplifiedWordOccGroupLst
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.getOrElse(wordOccurrences(word), Nil)

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil      => List(List())
//    case o :: Nil => {
//      val oC = o._1
//      val oI = o._2
//      println("combinations: oC: " + oC)
//      println("combinations: oI: " + oI)
//      val cKnt = (for { thsI <- 1 to oI by 1 } yield List((oC, thsI))).toList
//      println("combinations: cKnt: " + cKnt)
////      List(List()) ++ List(List(o))
//      List(List()) ++ cKnt
////      List(List()) ++ cKnt ++ List(List(o))
//    }
    case o :: ot  =>  {
//      println("combinations: occurences: " + occurrences)
//      println("combinations: o:" + o)
      val oC = o._1
      val oI = o._2
      val cKnt = (for { thsI <- 1 to oI by 1 } yield List((oC, thsI))).toList
//      println("combinations: cKnt: " + cKnt)

//      println("combinations: ot:" + ot)
      val combinationsOt = combinations(ot)
//      println("combinations: combinationsOt:" + combinationsOt)
      val combinationsOtWithO =
        (for { thsCKnt <- cKnt } yield combinationsOt flatMap (e => List(thsCKnt ++ e))).flatten
//        (for { thsCKnt <- cKnt } yield combinationsOt flatMap (e => List(e ++ thsCKnt))).flatten
//      println("combinations: combinationsOtWithO:" + combinationsOtWithO)
      val unsortedResults = combinationsOt ++ combinationsOtWithO
//      println("combinations: unsortedResults: " + unsortedResults)
      unsortedResults

//      val sortedResults = unsortedResults.sortWith((o1, o2) =>
//        (o1.length < o2.length) || ((o1.length == o2.length) && (o1.head._1 < o2.head._1)))
//      println("combinations:   sortedResults: " + sortedResults)
//      sortedResults
    }
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = y match {
//    val xSet = x.toSet
//    val ySet = y.toSet
//    println("sutract: xSet: " + xSet)
//    println("sutract: ySet: " + ySet)
//    assert(ySet subsetOf xSet)
//
//    (xSet -- ySet).toList

    case Nil => x
    case yo :: Nil => {
//      println("subtract: x: " + x + "; y: " + y)
      val partitionRes = x partition (xo => xo._1 == yo._1)
//      println("subtract: partitionRes: " + partitionRes)
      assert(partitionRes._1 != Nil)
      val xo = partitionRes._1.head
//      println("subtract: xo: " + xo)
      assert(xo._2 >= yo._2)
      val xot = partitionRes._2
//      println("subtract: xot: " + xot)

      if (xo._2 - yo._2 == 0) xot
      else {
        val xoNew = (xo._1, xo._2 - yo._2)
//        println("subtract: xoNew: " + xoNew)
        (xoNew :: xot).sortWith((o1, o2) => o1._1 <= o2._1)
      }
    }

    case yo :: yot => subtract(subtract(x, List(yo)), yot)
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    sentenceAnagramsInner(sentenceOccurrences(sentence))
  }

  def sentenceAnagramsInner(o: Occurrences): List[Sentence] = o match {
    case Nil => List(Nil)
    case oh :: ot => {
      val combs = combinations(o)
//      println("\nsentenceAnagramsInner: o: " + o + "; combs: " + combs)

      val nonEmptyCombs = combs filter (e => dictionaryByOccurrences.keySet contains e)
//      println("sentenceAnagramsInner: o: " + o + "; nonEmptyCombs: " + nonEmptyCombs)

      for (i <- nonEmptyCombs;
           j <- dictionaryByOccurrences(i);
           s <- sentenceAnagramsInner(subtract(o, i)))
        yield {
          //          println("sentenceAnagramsInner: o: " + o + "; loop j: " + j + "; s : " + s)
          j :: s
        }
    }
  }

//  def sentenceAnagrams(sentence: Sentence): List[Sentence] = sentence match {
//    case Nil => List(sentence)
//    case w :: wt => {
////      val dictChk = sentence forall  (wrd => dictionary contains wrd)
////      println("sentenceAnagrams: dictChk:" + dictChk)
////      assert(sentence forall (wrd => dictionary contains wrd))
//
//      val sentenceOcc = sentenceOccurrences(sentence)
//      println("sentenceAnagrams: sentenceOcc: " + sentenceOcc)
//
//      val sntOccCmb = combinations(sentenceOcc)
//      println("sentenceAnagrams: sntOccCmb: " + sntOccCmb)
//
//      val wrdLstOccLst = sntOccCmb map (e => dictionaryByOccurrences.get(e))
//      println("sentenceAnagrams: wrdLstOccLst: " + wrdLstOccLst)
//
//      val valWrdLstOccLst = wrdLstOccLst filter (e => e != None)
//      println("\nsentenceAnagrams: valWrdLstOccLst: " + valWrdLstOccLst)
//
//      val valWrdLstOccLst2 = valWrdLstOccLst map (e => e match {
//        case None => Nil
//        case Some(e2) => e2
//      })
//      println("\nsentenceAnagrams: valWrdLstOccLst2: " + valWrdLstOccLst2)
//
//      val wrdLstOccTplLst = valWrdLstOccLst2 map (e => (e, subtract(sentenceOcc, wordOccurrences(e.head))))
//      println("\nsentenceAnagrams: wrdLstOccTplLst: " + wrdLstOccTplLst)
//
//      val wrdAnmLstSntTplLst = wrdLstOccTplLst map (e => (e._1, buildSentences(e._2)))
//      println("\nsentenceAnagrams: wrdAnmLstSntTplLst: " + wrdAnmLstSntTplLst)
//
//      val wrdAnmLstValSntLstTplLst = wrdAnmLstSntTplLst filter (e => e._2 != Nil)
//      println("\nsentenceAnagrams: wrdAnmLstValSntLstTplLst: " + wrdAnmLstValSntLstTplLst)
//
//      val wrdAnmValSntLst = wrdAnmLstValSntLstTplLst map (e => e._1 map (wrd => wrd :: e._2))
//      println("\nsentenceAnagrams: wrdAnmValSntLst: " + wrdAnmValSntLst)
//
////      val completeSntLst = wrdAnmLstValSntLstTplLst map (e => e._2 map (snt => e._1 :: snt))
////      val completeSntLst = wrdAnmLstValSntLstTplLst flatMap (e => e._2 map (snt => e._1.head :: snt))
////      val completeSntLst = wrdAnmLstValSntLstTplLst flatMap (e => e._2 map (snt => e._1 map (wrd => wrd :: snt))
////      println("\nsentenceAnagrams: completeSntLst: " + completeSntLst)
////      completeSntLst
//
//      val sntOccCmbSnt = sntOccCmb flatMap (e => buildSentences(e))
////      println("sentenceAnagrams: sntOccCmbSnt: " + sntOccCmbSnt)
//      sntOccCmbSnt
//    }
//  }
//
//  def buildSentences(sentenceOcc: Occurrences): List[Sentence] = sentenceOcc match {
//    case Nil => Nil
//
//    case List(('m',1), ('y',1)) => List(List("my"))
//
//    case List(('a',1), ('m',1), ('n',1)) => List(List("man"))
//    case List(('a',1), ('s',1), ('y',1)) => List(List("say"))
//    case List(('e',1), ('m',1), ('n',1)) => List(List("men"))
//    case List(('e',1), ('s',1), ('y',1)) => List(List("yes"))
//
//    case List(('a',1), ('m',1), ('s',1), ('y',1)) => List(List("as", "my"), List("my", "as"))
//    case List(('a',1), ('e',1), ('n',1), ('s',1)) => List(List("as", "en"), List("en", "as"),
//                                                          List("sane"), List("sean"))
//    case List(('e',1), ('m',1), ('n',1), ('y',1)) => List(List("en", "my"), List("my", "en"))
//
//    case _ => {
//      println("buildSentences: sentenceOcc: " + sentenceOcc + "; return: " + "Nil")
//
//      Nil
//    }
//  }

  println("Sentence Anagrams for Trump Pence: " + sentenceAnagrams(List("Trump", "Pence")))

  val ClintonKaineAnm = sentenceAnagrams(List("Clinton", "Kaine"))
//  println("\nSentence Anagrams for Clinton Kaine: " + ClintonKaineAnm)
  println("\n# of Sentence Anagrams for Clinton Kaine: " + ClintonKaineAnm.length)

  val fltClintonKaineAnm = sentenceAnagrams(List("Clinton", "Kaine")) filter (e => !(
    (e.toSet contains "Acton")    ||
      (e.toSet contains "Al")    ||
      (e.toSet contains "Alec")    ||
    (e.toSet contains "Aleck")    ||
      (e.toSet contains "Ali")    ||
      (e.toSet contains "Alice")    ||
      (e.toSet contains "Alton")    ||
      (e.toSet contains "Ann")    ||
      (e.toSet contains "Anne")    ||
      (e.toSet contains "Annie")    ||
      (e.toSet contains "Anton")    ||
      (e.toSet contains "Cain")  ||
        (e.toSet contains "Caine")  ||
      (e.toSet contains "catlike")  ||
      (e.toSet contains "Celia")    ||
      (e.toSet contains "Celt")    ||
      (e.toSet contains "Clint")    ||
      (e.toSet contains "Clio")    ||
      (e.toSet contains "Cole")    ||
      (e.toSet contains "Conant")    ||
      (e.toSet contains "Conklin")    ||
      (e.toSet contains "Connie")    ||
      (e.toSet contains "Eli")    ||
      (e.toSet contains "en")    ||
      (e.toSet contains "et")    ||
      (e.toSet contains "Ian")    ||
      (e.toSet contains "Ike")    ||
      (e.toSet contains "Ilona")    ||
      (e.toSet contains "Io")    ||
      (e.toSet contains "Itel")    ||
      (e.toSet contains "Ito")    ||
      (e.toSet contains "Kane")    ||
      (e.toSet contains "Kant")    ||
      (e.toSet contains "Kent")    ||
      (e.toSet contains "Kiel")    ||
      (e.toSet contains "Klein")    ||
      (e.toSet contains "Kline")    ||
      (e.toSet contains "Lac")    ||
      (e.toSet contains "Lao")    ||
      (e.toSet contains "Leon")    ||
      (e.toSet contains "Len")    ||
      (e.toSet contains "Lena")    ||
      (e.toSet contains "Lin")    ||
      (e.toSet contains "Linton")    ||
      (e.toSet contains "Locke")    ||
      (e.toSet contains "Lockian")    ||
      (e.toSet contains "Loki")    ||
      (e.toSet contains "Nan")    ||
      (e.toSet contains "Nat")    ||
      (e.toSet contains "Nate")    ||
      (e.toSet contains "Neal")    ||
      (e.toSet contains "Neil")    ||
      (e.toSet contains "Nina")    ||
      (e.toSet contains "Noel")    ||
      (e.toSet contains "Nolan")    ||
      (e.toSet contains "Olin")    ||
      (e.toSet contains "Tieck")    ||
      (e.toSet contains "Tina")    ||
      (e.toSet contains "Toni")    ||
      false))
//  println("\nFiltered Sentence Anagrams for Clinton Kaine: " + fltClintonKaineAnm)
//  scala.tools.nsc.io.File("ClintonKaineAnagrams.txt").writeAll(fltClintonKaineAnm)

  val file = "ClintonKaineAnagrams.txt"
  val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))
  for (x <- fltClintonKaineAnm) {
    writer.write(x + "\n")  // however you want to format it
  }
  writer.close()

  println("\n# of Filtered Sentence Anagrams for Clinton Kaine: " + fltClintonKaineAnm.length)

}
