import java.util

import factorization.{Factorizer, PollardRho}
import org.scalatest._


class FactorTest extends FlatSpec {

  "13002" should "have 4 factors" in {
    val num: BigInt = BigInt(13002)
    val factors = Factorizer.factor(num, PollardRho)
    assert(factors.size == 4)
  }
}
