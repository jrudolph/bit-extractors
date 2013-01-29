package net.virtualvoid.binary.bits

import org.specs2.mutable.Specification

class BitPatternSpecs extends Specification {
  "BitPattern" should {
    "extract from ints" in {
      "match 1 bits" in {
        val Highest3Bits = BitPattern("111? ???? ???? ????")

        0xf500 must beLike { case Highest3Bits() => ok }
        0xa500 must not(beLike { case Highest3Bits() => ok })
      }
      "match 0 bits" in {
        val Highest3BitsAndZero = BitPattern("111? ?0?? ???? ????")

        0xf200 must beLike { case Highest3BitsAndZero() => ok }
        0xf500 must not(beLike { case Highest3BitsAndZero() => ok })
      }
      "extract entries" in {
        val Highest3BitsAndData = BitPattern("111a aaaa bbbb bbbb")

        // 1111 0101 0011 1000
        0xf538 must beLike { case Highest3BitsAndData(0x15, 0x38) => ok }
      }
      "extract flags" in {
        val Highest3BitsAndFlags = BitPattern("111A B???")

        0xe9 must beLike { case Highest3BitsAndFlags(false, true) => ok }
        0xc9 must not(beLike { case Highest3BitsAndFlags(false, true) => ok })
      }
    }
    "extract from byte sequences" in {
      def bytes(bs: Int*): Seq[Int] = bs.map(b => b & 0xff)

      "match 1 bits" in {
        val Highest3Bits = BitStreamPattern("111? ???? ???? ????")

        bytes(0xf5, 0) must beLike { case Highest3Bits(Nil, _*) => ok }
        bytes(0xa5, 0) must not(beLike { case Highest3Bits(Nil, _*) => ok })
      }
      "match 0 bits" in {
        val Highest3BitsAndZero = BitStreamPattern("111? ?0?? ???? ????")

        bytes(0xf2, 0) must beLike { case Highest3BitsAndZero(Nil, _*) => ok }
        bytes(0xf5, 0) must not(beLike { case Highest3BitsAndZero(Nil, _*) => ok })
      }
      "extract entries" in {
        val Highest3BitsAndData = BitStreamPattern("111a aaaa bbbb bbbb")

        // 1111 0101 0011 1000
        bytes(0xf5, 0x38) must beLike { case Highest3BitsAndData(Nil, 0x15, 0x38) => ok }
      }
      "extract flags" in {
        val Highest3BitsAndFlags = BitStreamPattern("111A B???")

        bytes(0xe9) must beLike { case Highest3BitsAndFlags(Nil, 0, 1) => ok }
        bytes(0xc9) must not(beLike { case Highest3BitsAndFlags(Nil, 0, 1) => ok })
      }
    }
  }
}
