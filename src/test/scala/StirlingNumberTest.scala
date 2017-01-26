import combinatorics.StirlingNumbers
import org.scalatest.FlatSpec

class StirlingNumberTest extends FlatSpec{

  var sn = StirlingNumbers.generateFirstKind(6, 3, true);
  println("stirling "+sn)

  var s2n = StirlingNumbers.generateFirstKind(5,2,true);
  println("stirling "+s2n)
}
