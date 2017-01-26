import combinatorics.{Permutations, CatalanNumber}
import org.scalatest.FlatSpec

class CatalanNumberTest extends FlatSpec {

  "4th catalan number" should "be 14" in{
   val l = new CatalanNumber(4).number.get
    assert(l == 14)
  }

  "5th catalan number" should "be 42" in{
    val l = new CatalanNumber(5).number.get
    assert(l == 42)
  }

  "11th catalan number" should "be 58786" in{
    val l = new CatalanNumber(11).number.get
    assert(l == 58786)
  }

  "16th catalan number" should "be 35357670" in{
    val l = new CatalanNumber(16).number.get
    println("cat 16 is       "+l)
    assert(l == 35357670)
  }


  "Catalan rank of (0010110101) " should "be 22" in {
    val b = new CatalanNumber(5)
    val c = b.catalanRank(Array(false, false, true, false, true, true, false, true, false, true))
    assert(c.get == 22)
  }
}
