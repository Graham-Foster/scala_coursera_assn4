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
    val t_assignment = Fork(Leaf('A',8),Fork(Fork(Leaf('B',3),Fork(Leaf('C',1),Leaf('D',1),List('C', 'D'),2),List('B', 'C', 'D'),5),Fork(Fork(Leaf('E',1),Leaf('F',1),List('E', 'F'),2),Fork(Leaf('G',1),Leaf('H',1),List('G', 'H'),2),List('E', 'F', 'G', 'H'),4),List('B', 'C', 'D', 'E', 'F', 'G', 'H'),9),List('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),17)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of a single leaf tree") {
    val t = Leaf('a', 3)
    assert(weight(t) === 3)
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("times") {
    val list = List('a', 'b', 'c', 'b', 'a', 'a')
    assert(times(list).head === ('a', 3))
    assert(times(list).tail.head === ('b', 2))
    assert(times(list).tail.tail.head === ('c', 1))
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("keep combining until done") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) === Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Leaf('x',4),List('e', 't', 'x'),7))
  }

  test("createCodeTree from assignment example") {
    val word = "AAAAAAAABBBCDEFGH";
    assert(createCodeTree(word.toList) === Fork(Leaf('A',8),Fork(Fork(Fork(Leaf('C',1),Leaf('D',1),List('C', 'D'),2),Fork(Leaf('E',1),Leaf('F',1),List('E', 'F'),2),List('C', 'D', 'E', 'F'),4),Fork(Fork(Leaf('G',1),Leaf('H',1),List('G', 'H'),2),Leaf('B',3),List('G', 'H', 'B'),5),List('C', 'D', 'E', 'F', 'G', 'H', 'B'),9),List('A', 'C', 'D', 'E', 'F', 'G', 'H', 'B'),17))
  }

  test("decode from assignment example") {
    new TestTrees {
      val bits: List[Bit] = List(0,1,0,0,1,0,1,0,1,0,1,1,1,1,0,0,1,1,0,1,1,1,1,0,1,1,1,1)
      assert(decode(t_assignment, bits) === List('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'))
    }
  }

  test("What is the french secret") {
    assert(decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("codeBits") {
    val table: CodeTable = List(('A', List(0)), ('B', List(1,0)), ('C', List(1,1)))
    assert(codeBits(table)('A') === List(0))
    assert(codeBits(table)('B') === List(1,0))
    assert(codeBits(table)('C') === List(1,1))
  }

  test("Convert code tree to code table") {
    val tree = Fork(Leaf('A', 4), Fork(Leaf('B', 2), Leaf('C', 1), List('B', 'C'), 3), List('A', 'B', 'C'), 7)
    val table: CodeTable = List(('A', List(0)), ('B', List(1,0)), ('C', List(1,1)))
    assert(convert(tree) === table)
  }

  test("quickEncode from assignment example") {
    new TestTrees {
      val bits: List[Bit] = List(0,1,0,0,1,0,1,0,1,0,1,1,1,1,0,0,1,1,0,1,1,1,1,0,1,1,1,1)
      assert(decode(t_assignment, quickEncode(t_assignment)("ABCDEFGH".toList)) === "ABCDEFGH".toList)
    }
  }
}
