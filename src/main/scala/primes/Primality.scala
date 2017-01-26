package primes


object Primality {

  def isPrime(n : Long) : Boolean = {
    if (n <= 1)
    return false
    if (n == 2)
    return true

    if (n % 2 == 0)
      return false

    if(n % 3 == 0)
      return false

    if(n % 5 == 0)
      return false

    val rangeMax = Math.sqrt(n).asInstanceOf[Int] + 1
    var i = 7
    while (i < rangeMax){
      if (n % i == 0) {
        return false
      }
      i += 2
    }
    return true
  }

}
