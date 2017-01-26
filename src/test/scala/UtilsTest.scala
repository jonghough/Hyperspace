import org.scalatest.FlatSpec
import utils.Utils


class UtilsTest extends FlatSpec{

  {
    val i = Utils.factorial32(3)
    assert(i == 6)
  }

  {
    val i = Utils.factorial32(4)
    assert(i == 24)
  }
}
