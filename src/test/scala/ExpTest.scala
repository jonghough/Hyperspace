import org.scalatest.FlatSpec
import utils.{Exp, Utils}


class ExpTest extends FlatSpec  {
  var t = Exp.bigExp(BigDecimal("2.71828"), 3.75);
  System.out.println("++++++++++++++++++++++++++++++ exponent e^3.75 is  "+t)

  var g = Utils.nroot(BigDecimal("50.343"), 10)
  System.out.println(" +++++++++++++++++++++++++++++++++++ g is  "+g)

  System.out.println(" +++++++++++++++++++++++++++++++ ln (14.342 is  "+Exp.ln(14.342, 100))

  "Difference between 2^10 and 1024" should "be < 0.001" in {
    var f = Exp.bigExp(BigDecimal(2), BigDecimal(10))
    assert((f - 1024).abs < 0.001)
  }
}
