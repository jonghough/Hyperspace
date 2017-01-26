package primes

import utils.Utils


object PrimeCount {

  def calcNthPrime(n : BigInt) : BigInt = {

    if (n <= 0)
      throw new IllegalArgumentException("Argument must be positive.");
    if (n == 1)
      return 2
    var prime : BigInt = 2

    var i = 0
    var count : BigInt = 3
    i = 1

    while (i < n) {
      if (count.isProbablePrime(100)) {
        prime = count
        i+=1
      }
      count+=1
    }
    return prime
  }



  def phi(m : BigDecimal, n : BigInt) : BigInt = {
    if (m < 0 || n < 0)
      throw new IllegalArgumentException("Arguments must be non-negative");
    if (n == 0) {
      m.toBigInt()
    } else {
      var np = BigDecimal(calcNthPrime(n))
      var k = n - 1
      return phi(m, k) - phi(m / np, k)
    }
  }

  def pi(d : BigDecimal) : BigInt = {
    if(d < 2) 0
    else if(d < 3) 1
    else if(d < 5) 2
    else if(d < 7) 3
    else if(d < 11) 4
    else{
      val cbroot = pi(Utils.cbrt(d))
      val mu = pi(Utils.sqrt(d)) - cbroot
      phi(d, cbroot) + cbroot * (mu + 1) + (BigDecimal(mu * mu - mu) * 0.5).toBigInt() - 1 - sumPi(d, cbroot, mu)
    }
  }

  def sumPi(d : BigDecimal, n : BigInt, mu : BigInt) : BigInt = {
    var i : BigInt = 1
    var total : BigInt = 0

    while(i <= mu){
      total += pi(d / BigDecimal(calcNthPrime(n + i)))
      i += 1
    }
    total
  }

}
