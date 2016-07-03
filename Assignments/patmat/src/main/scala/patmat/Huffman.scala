package patmat

import common._

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree {
//    override def toString = "\nFork(" + left + ",\n     " + right + ",\n     " + chars + ", " + weight + ")\n"
  }
  case class Leaf(char: Char, weight: Int) extends CodeTree

  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    // tree match ...
    case Leaf(char, weight) => weight
    case Fork(left, right, chars, weight) => weight
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(char, weight) => List(char)
    case Fork(left, right, chars, weight) => chars
  }

  def left(tree: CodeTree): CodeTree = tree match {
    case Leaf(char, weight) => throw new Error("left of Leaf: " + tree + "; undefined")
    case Fork(left, right, chars, weight) => left
  }

  def right(tree: CodeTree): CodeTree = tree match {
    case Leaf(char, weight) => throw new Error("right of Leaf: " + tree + "; undefined")
    case Fork(left, right, chars, weight) => right
  }

  // makeCodeTree should maintain weight sorting of branches ???
  def makeCodeTree(left: CodeTree, right: CodeTree) =
    if (weight(left) <= weight(right))
      Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))
    else
      Fork(right, left, chars(right) ::: chars(left), weight(right) + weight(left))

  def isLeaf(tree: CodeTree): Boolean = tree match {
    case Leaf(char, weight) => true
    case Fork(left, right, chars, weight) => false
  }

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def times(chars: List[Char]): List[(Char, Int)] = chars match {
    case List()  => Nil
//      throw new Error("times: empty list")
    case List(c) => List((c, 1))
    case c :: cs => {
      val tailRes = times(cs).partition(e => e._1 == c)
      // tailRes is a tuple of Lists

      if (tailRes._1.isEmpty) (c, 1) :: tailRes._2
      else (c, tailRes._1.head._2 + 1) :: tailRes._2
    }
  }

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs match {
    case List()  => Nil
//      throw new Error("makeOrderedLeafList: empty list")
    case List(f) => List(new Leaf(f._1, f._2))
    case f :: fs => (new Leaf(f._1, f._2) :: makeOrderedLeafList(fs)).sortWith((e1, e2) =>
      (e1.weight  < e2.weight) || ((e1.weight == e2.weight) && (e1.char < e2.char)))
  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case List()  => false
//      throw new Error("singleton: empty list")
    case t :: ts => ts.isEmpty
  }

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case List()   => Nil
//      throw new Error("combine: empty list")
    case t :: Nil => trees
    case t :: ts :: trest => (makeCodeTree(t, ts) :: trest).sortWith((e1, e2) => weight(e1) < weight(e2))
  }

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
  def until(singletonFn: List[CodeTree] => Boolean,
            combineFn:   List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] =
    trees match {
      case Nil      => throw new Error("until: empty trees")
      case t :: Nil => trees
      case t :: tt  => until(singletonFn, combineFn)(combineFn(trees))
  }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = chars match {
    case Nil      => throw new Error("until: empty trees")
    case c :: Nil => new Leaf(c, 1)
    case c :: ct  => until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  }
//  def createCodeTree(chars: List[Char]): CodeTree = {
//    //    println("createCodeTree: chars: " + chars)
//    val freqsLst = times(chars)
//    //    println("createCodeTree:   freqsLst: " + freqsLst)
//    val leafLst = makeOrderedLeafList(freqsLst)
//    //    println("createCodeTree:   leafLst: " + leafLst)
//    until(singleton, combine)(leafLst).head
//  }


  // Part 3: Decoding

  type Bit = Int

  /**
    * This function splits the bit sequence `bits` using the code tree `tree` and returns
    * the resulting list of `bits` for each char.
    */
//  def splitBits(tree: CodeTree, bits: List[Bit]): List[List[Bit]] = {
//    if (isLeaf(tree)) {
//      val retVal = if (bits.length == 0) List() else List() :: splitBits(tree, bits)
//      println("splitBits: bits: " + bits + "; retVal: " + retVal)
//      retVal
//    }
//    else {
//      assert((bits.head == 0) || (bits.head == 1), "splitBits: bits.head : " + "; should be 0 or 1")
//      if (bits.head == 0) splitBits( left(tree), bits.tail) else
//      if (bits.head == 1) splitBits(right(tree), bits.tail) else
//      throw new Error("splitBits: bits.head : " + "; should be 0 or 1")
//    }
//  }
//
//  /**
//   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
//   * the resulting list of ONE character.
//   */
//  def decodeOneChar(tree: CodeTree, bits: List[Bit]): List[Char] = {
//    if (isLeaf(tree)) {
//      if (bits.length > 0)
//        println("decodeOneChar: returning: " + chars(tree) + "; remaining bits: " + bits)
//      chars(tree)
//    }
//    else {
//      assert((bits.head == 0) || (bits.head == 1), "decodeOneChar: bits.head : " + "; should be 0 or 1")
//      if (bits.head == 0) decodeOneChar( left(tree), bits.tail) else
//      if (bits.head == 1) decodeOneChar(right(tree), bits.tail) else
//      throw new Error("decodeOneChar: bits.head : " + "; should be 0 or 1")
//    }
//  }

  /**
    * This function decodes the bit sequence `bits` using the code tree `tree` and returns
    * the resulting list of characters.
    */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = bits match {
    case Nil => throw new Error("decode: empty bits")
    case _   => decodeAccumulator(tree, tree, bits, List())
  }

  private def decodeAccumulator(orgTree: CodeTree, tree: CodeTree, bits: List[Bit],
                                message: List[Char]): List[Char] = {
//    println("decodeAccumulator: entry: tree: " + tree +
//      "; bits: " + bits + "; message: " + message)
    tree match {
      case Leaf(char, _) => {
        //      println("decodeAccumulator: Leaf: orgTree: " + orgTree + "; tree: " + tree +
        //                                        "; bits: " + bits + "; message: " + message)
        //      println("decodeAccumulator: Leaf: entry: tree: " + tree +
        //        "; bits: " + bits + "; message: " + message)
        bits match{
          case Nil      => {
//            println("decodeAccumulator: Leaf: bits: Nil: " + bits + " return: " + message + " ++ " + List(char))
            message ++ List(char)
          }
          case b :: _ => decodeAccumulator(orgTree, orgTree, bits, message ++ List(char))
        }
      }
      case Fork(left, right, _, _) => bits match {
        case Nil      => throw new Error("decodeAccumulator: empty bits for Fork")
        case 0 :: bt  => decodeAccumulator(orgTree, left,  bt, message)
        case 1 :: bt  => decodeAccumulator(orgTree, right, bt, message)
        case _ :: bt  => throw new Error("decodeAccumulator: unexpected bits.head: " + bits.head + "  for Fork")
//        case _ => message
      }
    }
  }

  //  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
//    val thsBitsLst = splitBits(tree, bits)
//    //    val thsBitsLst = List(List(1,0,0), List(0), List(1,0,1,0))
//    thsBitsLst.flatMap(decodeOneChar(tree, _))
//  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
    **/
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)
  println("decoded Secret: Message: " + decodedSecret)

  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = text match {
    case Nil    => throw new Error("encode: empty text")
    case t::Nil => encodeOneChar(tree)(t)
    case t::tt  => encodeOneChar(tree)(t) ::: encode(tree)(tt)
  }

  private def encodeOneChar(tree: CodeTree)(textChar: Char): List[Bit] = tree match {
    case Leaf(char, _) =>
      if (char == textChar) Nil
      else throw new Error("encodeOneChar: textChar: " + textChar + " does not match Leaf char: " + tree)
    case Fork(left, right, forkChars, _) => {
           if (chars(left ) contains textChar) 0 :: encodeOneChar(left )(textChar)
      else if (chars(right) contains textChar) 1 :: encodeOneChar(right)(textChar)
      else throw new Error("encodeOneChar: textChar: " + textChar + " does not match Fork char: " + forkChars)
    }
  }
  
  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table.find(e => e._1 == char).head._2
  
  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = tree match {
    case Leaf(_, _)       => Nil
    case Fork(l, r, _, _) => convertAccumulator(l, List(0)) ++ convertAccumulator(r, List(1))
  }

  private def convertAccumulator(tree: CodeTree, bits: List[Bit]): CodeTable = {
    //    println("decodeAccumulator: entry: tree: " + tree +
    //      "; bits: " + bits + "; message: " + message)
    tree match {
      case Leaf(char, _) => {
        //      println("decodeAccumulator: Leaf: orgTree: " + orgTree + "; tree: " + tree +
        //                                        "; bits: " + bits + "; message: " + message)
        //      println("decodeAccumulator: Leaf: entry: tree: " + tree +
        //        "; bits: " + bits + "; message: " + message)
        List((char, bits))
      }
      case Fork(l, r, _, _) => convertAccumulator(l, bits ++ List(0)) ++ convertAccumulator(r, bits ++ List(1))
    }
  }


  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ++ b
  
  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val thsCodeTable = convert(tree)
    text.flatMap(codeBits(thsCodeTable))
  }
}
