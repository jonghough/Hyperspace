import arithmetic.Sigma
import org.scalatest.FlatSpec


class SigmaTest extends FlatSpec{

  "Sigma(0,230) " should "be 8" in {
    val p = Sigma.sigma(0,230)
    assert(p == 8)
  }

  "Sigma(0,7) " should "be 2" in {
    val p = Sigma.sigma(0,7)
    assert(p == 2)
  }

  "Sigma(1,12) " should "be 28" in {
    val p = Sigma.sigma(1,12)
    assert(p == 28)
  }
}
