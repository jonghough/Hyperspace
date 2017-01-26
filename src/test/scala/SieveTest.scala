import arithmetic.Sieve
import org.scalatest.FlatSpec

class SieveTest extends FlatSpec{

  "Eratosthense Sieve: Sieved 100, number of primes less than or equal to 100" should "25" in{
    val s = Sieve.eratosthenes(100)
    assert(s.size == 25)
  }

  "Atkin Sieve: Sieved 100, number of primes less than or equal to 100" should "25" in{
    val s = Sieve.atkin(100)
    assert(s.size == 25)
  }

  "Atkin Sieve: Sieved 1000, number of primes less than or equal to 1000" should "168" in{
    val s = Sieve.atkin(1000)
    assert(s.size == 168)
  }

  "Eratosthenes Sieve: Sieved 1000, number of primes less than or equal to 1000" should "168" in{
    val s = Sieve.eratosthenes(1000)
    assert(s.size == 168)
  }
}
