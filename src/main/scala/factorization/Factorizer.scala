package factorization

import java.util

import scala.collection.mutable.ArrayBuffer

/**
 *
 */
object Factorizer {

  /**
   * Factors the <code>BigInt<code> value N, into prime factors, using the given
   * factorization function. The factorization function can be one of several
   * functions, e.g. <i>Pollard Rho</i>, <i>Lenstra</i> etc.
    *
    * @param N Big Integer to factor
   * @param factorFunc Factorization function
   * @return List of prime factors (unordered)
   */
  def factor(N: BigInt, factorFunc: Factorable ): ArrayBuffer[BigInt]={

    var factors = new ArrayBuffer[BigInt]
    if(N == 1)
      return factors
    else {
      var d = N
      while (d != 1) {
        val divisor: BigInt = factorFunc.factor(d)
        d = d / divisor
        factors += divisor
        //f//actors ++= (factor(N / divisor, factorFunc))

      }
      return factors
    }

  }
}
