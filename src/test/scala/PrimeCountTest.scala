import org.scalatest.FlatSpec
import primes.PrimeCount

class PrimeCountTest extends FlatSpec {

  "5th prime" should "be 11" in {
    val e = PrimeCount.calcNthPrime(5)
    assert(e == 11)
  }

  "number of primes less than or equal to 100000" should "be 9592" in {
    val d = PrimeCount.pi(100000)
    assert(d == 9592)
  }

  "number of primes less than or equal to 100" should "be 25" in {
    val d = PrimeCount.pi(100)
    assert(d == 25)
  }
}
