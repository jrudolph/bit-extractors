package net.virtualvoid.binary.bits

trait BitPattern {
  def unapplySeq(i: Int): Option[Seq[Any]]
}

object BitPattern {
  def apply(pattern: String): BitPattern = {
    val cleanedUp = pattern.filterNot(_ == ' ')

    def mask(cand: Char): Int =
      cleanedUp.foldLeft(0) { (mask, c) =>
        val value = if (c == cand) 1 else 0
        (mask << 1) | value
      }

    val numBits = cleanedUp.size
    val onePattern = mask('1')
    val zeroPattern = mask('0')
    val special = "10?".toSet

    def positionsFor(c: Char): Seq[Int] =
      (0 until numBits).filter(cleanedUp(_) == c)
    val extracts =
      cleanedUp.distinct.filterNot(special).map(positionsFor).sortBy(_.head)

    require(numBits <= 32)

    def collectBits(bits: Seq[Int]): Int =
      bits.foldLeft(0) { (bits, bit) =>
        (bits << 1) | bit
      }

    new BitPattern {
      def unapplySeq(i: Int): Option[Seq[Any]] = {
        def get(bit: Int): Int =
          (i & (1 << (numBits - bit - 1))) >> (numBits - bit - 1)
        def extract(bits: Seq[Int]): Any =
          if (bits.size == 1)
            (i & (1 << (numBits - bits.head - 1))) != 0
          else
            collectBits(bits.map(get))

        val allMatched =
          ((i & onePattern) == onePattern) &&
          (~i & zeroPattern) == zeroPattern

        if (allMatched)
          Some(extracts.map(extract))
        else
          None
      }
    }
  }
}

trait BitStreamPattern {
  def unapplySeq(bs: Seq[Int]): Option[(Seq[Int], Seq[Int])]
}
object BitStreamPattern {
  def apply(pattern: String): BitStreamPattern = {
    val cleanedUp = pattern.filterNot(_ == ' ')
    def numBits = cleanedUp.size
    require(numBits % 8 == 0)
    val numBytes = numBits / 8

    def byteMask(p: Char => Boolean): Seq[Int] =
      cleanedUp.grouped(8).map { bs =>
        bs.foldLeft(0) { (mask, c) =>
          val value = if (p(c)) 1 else 0
          (mask << 1) | value
        }
      }.toSeq

    val special = "10?".toSet

    def positionsFor(c: Char): Seq[Int] =
      (0 until numBits).filter(cleanedUp(_) == c)
    val extracts =
      cleanedUp.distinct.filterNot(special).map(positionsFor).sortBy(_.head)

    val OnePatterns = byteMask('1' ==).toArray
    val TotalPatterns = byteMask(x => x == '0' || x == '1').toArray

    def collectBits(bits: Seq[Int]): Int =
      bits.foldLeft(0) { (bits, bit) =>
        (bits << 1) | bit
      }

    new BitStreamPattern {
      def unapplySeq(bs: Seq[Int]): Option[(Seq[Int], Seq[Int])] = {
        def get(idx: Int): Int = {
          val byte = idx / 8
          val bit = idx % 8
          val i = bs(byte)
          (i & (1 << (8 - bit - 1))) >> (8 - bit - 1)
        }

        def extract(bits: Seq[Int]): Int =
          collectBits(bits.map(get))

        def matches(idx: Int): Boolean =
          ((bs(idx) ^ OnePatterns(idx)) & TotalPatterns(idx)) == 0

        if (bs.size < numBytes)
          None
        else {
          val allMatched =
            (0 until numBytes).forall(matches)

          if (allMatched)
            Some((bs.drop(numBytes), extracts.map(extract)))
          else
            None
        }
      }
    }
  }
}
