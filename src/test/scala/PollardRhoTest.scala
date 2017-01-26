import factorization.PollardRho
import org.scalatest._


class PollardRhoTest extends FlatSpec {

  "8" should "have a prime factor greater than 1 " in {
    val num: BigInt = BigInt(8)
    val k = PollardRho.factor(num)
    assert(PollardRho.factor(k) > 1)
  }
}
