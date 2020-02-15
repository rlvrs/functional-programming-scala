package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    val tEFGH = Fork(
      Fork(Leaf('e', 1), Leaf('f', 1), List('e', 'f'), 2),
      Fork(Leaf('g', 1), Leaf('h', 1), List('g', 'h'), 2),
      List('e', 'f', 'g', 'h'), 4)
    val tBCD = Fork(
      Leaf('b', 3),
      Fork(Leaf('c', 1), Leaf('d', 1), List('c', 'd'), 2),
      List('b', 'c', 'd'), 5)
    val tBCDEFGH =
      Fork(
        Fork(
          Leaf('b', 3),
          Fork(
            Leaf('c', 1),
            Leaf('d', 1),
            List('c', 'd'), 2),
          List('b', 'c', 'd'), 5
        ),
        Fork(
          Fork(
            Leaf('e', 1),
            Leaf('f', 1),
            List('e', 'f'), 2),
          Fork(
            Leaf('g', 1),
            Leaf('h', 1),
            List('g', 'h'), 2),
          List('e', 'f', 'g', 'h'), 4
        ),
        List('b', 'c', 'd', 'e', 'f', 'g', 'h'), 9
      )
  }

  trait TestTables {
    val table1: CodeTable = List('a' -> List(0),'b' -> List(1))
    val table2: CodeTable = List('a' -> List(0),'b' -> List(0),'d' -> List(1))
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("times") {
    assert(times(string2Chars("aba")) === List(('a', 2), ('b', 1)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("the decoded secret must decode huffmanestcool") {
    assert(decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }

  /*
  test("merge codetables correctly") {
    new TestTables {
      assert(mergeCodeTables(table1, table2) === List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))
    }
  }
  */

  test("convert: codetable is created correctly") {
    new TestTrees {
      assert(convert(t2) === List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))
      assert(convert(tEFGH) === List(('e', List(0, 0)), ('f', List(0, 1)), ('h', List(1, 1)), ('g', List(1, 0))))
      assert(convert(tBCD) === List(('b', List(0)), ('d', List(1, 1)), ('c', List(1, 0))))
      assert(convert(tBCDEFGH) === List(
        ('d', List(0, 1, 1)),
        ('c', List(0, 1, 0)),
        ('b', List(0, 0)),
        ('h', List(1, 1, 1)),
        ('g', List(1, 1, 0)),
        ('f', List(1, 0, 1)),
        ('e', List(1, 0, 0))
      ))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and quick encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t1, quickEncode(t1)("abbaba".toList)) === "abbaba".toList)
    }
  }

  test("encode gives the correct byte sequence") {
    assert(encode(frenchCode)(string2Chars("huffmanestcool")) === secret)
  }

  test("quick encode gives the correct byte sequence") {
    assert(quickEncode(frenchCode)(string2Chars("huffmanestcool")) === secret)
  }
}
