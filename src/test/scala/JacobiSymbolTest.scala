import arithmetic.JacobiSymbol
import org.scalatest._

class JacobiSymbolTest extends FlatSpec{
  "Jacobi symbol (100/201)" should "equal 1" in {
    var js = JacobiSymbol.calculate(BigInt("100"), BigInt("201"))
    System.out.println(js + "ok")
    assert(js == 1)
  }
}
