import org.scalatest.FlatSpec
import transcendence.Gamma

class GammaTest extends FlatSpec{

  "Gamma(11)" should "be 3628800" in{
    val g = Gamma.gamma(11.0)
    assert((g - 3628800).abs < 0.001 )
  }

  "LogGamma(4.5)" should "be 2.4537" in{
    val g = Gamma.logGamma(4.5)
    assert((g - 2.4537365).abs < 0.001 )
  }

  "Gamma(0.5)" should "be 1.77245" in{
    val g = Gamma.gamma(0.5)
    assert((g - 1.77245).abs < 0.001 )
  }
}
