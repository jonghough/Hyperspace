import factorization.{Factorizer, PPO}
import org.scalatest.FlatSpec


class PPOTest extends FlatSpec {

  "27326530 " should "have 6 factors" in {
    val f = Factorizer.factor(27326530, PPO)
    assert(f.size == 6)
  }
}
