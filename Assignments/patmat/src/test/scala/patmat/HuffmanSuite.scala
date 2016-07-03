package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(times(List()) === List())
    assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
    assert(makeOrderedLeafList(List()) === List())
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist3 = List(Leaf('x', 4), Leaf('t', 2), Leaf('e', 1))
    assert(!singleton(Nil))
    assert(singleton(List(Leaf('e', 1))))
    assert(!singleton(leaflist3))
    assert(combine(Nil) == Nil)
    assert(combine(List(Leaf('x', 4))) == List(Leaf('x', 4)))
    assert(combine(leaflist3) === List(
      Leaf('e', 1),
      Fork(Leaf('t',2),Leaf('x',4),List('t', 'x'),6)
    ))

    val leaflist4 = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4), Leaf('y', 8))
    val treelist4 = combine(leaflist4)
    assert(treelist4 === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),
                              Leaf('x',4),Leaf('y',8)))
    assert(until(singleton, combine)(treelist4) === List(
      Fork(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),
                Leaf('x',4),
                List('e','t','x'), 7),
           Leaf('y',8),
           List('e','t','x','y'), 15)
    ))
  }

  test("create code tree for a List[char]") {
    val smpLst = List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')
    val smpTree = createCodeTree(smpLst)
    assert(smpTree ===
      Fork(Fork(Leaf('o',2),
                Fork(Leaf('w',1),
                     Fork(Leaf('h',1),Leaf('r',1),List('h', 'r'),2),
                     List('w', 'h', 'r'),3),
                List('o', 'w', 'h', 'r'),5),
           Fork(Leaf('l',3),
                Fork(Fork(Leaf('d',1),Leaf('e',1),List('d', 'e'),2),
                          Fork(Leaf(' ',1),Leaf(',',1),List(' ',','),2),
                          List('d', 'e',' ',','),4),
                List('l', 'd', 'e',' ',','),7),
           List('o', 'w', 'h', 'r', 'l', 'd', 'e',' ', ','),12)
//      Fork(
//        Fork(Leaf(' ',1),Leaf(',',1),List(' ',','),2),
//        Fork(
//          Fork(Leaf('d',1),Leaf('e',1),List('d','e'),2),
//          Fork(
//            Fork(Leaf('h',1),Leaf('r',1),List('h','r'),2),
//            Fork(Leaf('l',3),
//                 Fork(Leaf('w',1),Leaf('o',2),List('w','o'),3),
//                 List('l','w','o'),6),
//            List('h','r','l','w','o'),8),
//          List('d','e','h','r','l','w','o'),10),
//        List(' ',',','d','e','h','r','l','w','o'),12)
    )
  }


  test("decode and encode ABCDEFGH") {
    new TestTrees {
      val smpTree =
            Fork(Leaf('A', 8),
                 Fork(Fork(Leaf('B', 3),
                           Fork(Leaf('C', 1),Leaf('D', 1),List('C','D'), 2),
                           List('B','C','D'), 5),
                      Fork(Fork(Leaf('E', 1),Leaf('F', 1),List('E','F'), 2),
                           Fork(Leaf('G', 1),Leaf('H', 1),List('G','H'), 2),
                           List('E','F','G','H'), 4),
                      List('B','C','D','E','F','G','H'), 9),
                 List('A','B','C','D','E','F','G','H'), 17)
      val smpBits = List(1,0,0,0,1,0,1,0)
      assert(decode(smpTree, smpBits) === List('B','A','C'))
      val smp2Tree =
        Fork(Leaf('A', 1),
             Fork(Leaf('B', 1),
                  Fork(Leaf('C', 1),Leaf('D', 1),List('C','D'), 2),
                  List('B','C','D'), 3),
              List('A','B','C','D'), 4)

      val smp2Bits = List(1,0,0,1,1,0)
      assert(decode(smp2Tree, smp2Bits) === List('B','A','C'))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(encode(t1)("ab".toList) === List(0, 1))
      assert(decode(t1, List(0, 1)) == List('a', 'b'))
      assert(decode(t1, List(0, 1, 1)) == List('a', 'b', 'b'))
      assert(decode(t1, List(0, 1, 0)) == List('a', 'b', 'a'))
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("CodeTable suite") {
    new TestTrees {
      val smp2Tree =
        Fork(Leaf('A', 1),
          Fork(Leaf('B', 1),
            Fork(Leaf('C', 1),Leaf('D', 1),List('C','D'), 2),
            List('B','C','D'), 3),
          List('A','B','C','D'), 4)

      val smp2Table = List( ('A', List(0)),
                            ('B', List(1, 0)),
                            ('C', List(1, 1, 0)),
                            ('D', List(1, 1, 1))
                          )
      assert(codeBits(smp2Table)('A') === List(0))
      assert(codeBits(smp2Table)('B') === List(1, 0))
      assert(codeBits(smp2Table)('C') === List(1, 1, 0))
      assert(codeBits(smp2Table)('D') === List(1, 1, 1))

      assert(convert(smp2Tree) === smp2Table)

      assert(mergeCodeTables(
        List( ('A', List(0)),
          ('B', List(1, 0))
            ),
          List(
          ('C', List(1, 1, 0)),
          ('D', List(1, 1, 1))
              )
                            ) === smp2Table)

      assert(quickEncode(smp2Tree)("DCBA".toList) === List(1, 1, 1, 1, 1, 0, 1, 0, 0))
    }
  }
}
