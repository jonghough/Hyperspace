import org.scalatest.FlatSpec
import transcendence.Zeta

class ZetaTest extends FlatSpec{

  "Z(21.022...)" should "be 0" in {
    val d = new Zeta().z(21.0220396387715)
    assert(Math.abs(d - 0.001) < 0.001)
  }

  "Z(14.134725...)" should "be 0" in {
    val d = new Zeta().z(14.13472514173469)
    assert(Math.abs(d - 0.001) < 0.001)
  }

  "Z(143.1118458...)" should "be 0" in {
    val d = new Zeta().z(143.1118458076206)
    assert(Math.abs(d - 0.001) < 0.001)
  }
}
