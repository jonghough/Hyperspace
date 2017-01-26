package factorization

import arithmetic.EllipticCurve


object Lenstra extends Factorable {

  override def factor(N: BigInt) : BigInt = {
    return EllipticCurve.factorLenstra(N)
  }
}
