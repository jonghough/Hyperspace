package factorization


import scala.BigInt._
import scala.util.Random


object PollardRho extends Factorable {

  var rand: Random = new Random(System.currentTimeMillis());

  override def factor(N: BigInt) : BigInt = {
    if(N == 1)
        return 1
    else if(N == 2)
        return 2
    else if(N == 3)
      return 3
    else if(N == 4)
      return 2;
    else if(N.isProbablePrime(90)){
      return N
    }
    else {
      while (true) {
        var a: BigInt = BigInt(N.bitLength, rand)
        var sum: BigInt = BigInt(N.bitLength, rand) + 1
        var b: BigInt = a
        a = calcValue(a, sum, N)
        b = calcValue(calcValue(b, sum, N), sum, N)
        var divisor: BigInt = (a - b).gcd(N)
        var i = 0;
        while (i < 100000 && divisor == 1) {
          a = calcValue(a, sum, N)
          b = calcValue(calcValue(b, sum, N), sum, N)
          divisor = (a - b).gcd(N)
        }

        if (divisor.isProbablePrime(100)) {
          return divisor;
        }
        else return PollardRho.factor(divisor);
      }
      return N;
    }
  }

  private def calcValue(n : BigInt, summand : BigInt, N : BigInt): BigInt ={
    return (n * n + summand ).mod(N);
  }

}