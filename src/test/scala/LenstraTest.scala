import factorization.{Factorizer, Lenstra}
import org.scalatest._

class LenstraTest extends FlatSpec {

  "3002351" should "have 3 factors" in {
    val factors = Factorizer.factor(BigInt("3002351"), Lenstra)
    for (i <- 0 until factors.size) {
      System.out.println("FACTOR >>>> " + factors(i))
    }

    System.out.println("305 factors # = " + factors.size)
    assert(factors.size == 3)
  }
}
