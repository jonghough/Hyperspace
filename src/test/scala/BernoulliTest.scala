import arithmetic.BernoulliNumbers
import org.scalatest.FlatSpec

class BernoulliTest  extends FlatSpec {

" First 4 Bernoulli numbers " should "be 1, 1/6, -1/30, 1/42" in {
  val bNumbers = BernoulliNumbers.generateBernoulliNumbers(10)
  assert(bNumbers(0)._numerator == 1);
  assert(bNumbers(1)._numerator == 1);
  assert(bNumbers(1)._denominator == 6);
  assert(bNumbers(2)._numerator == -1);
  assert(bNumbers(2)._denominator == 30);
  assert(bNumbers(3)._numerator == 1);
  assert(bNumbers(3)._denominator == 42);

}
}
