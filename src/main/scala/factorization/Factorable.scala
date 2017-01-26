package factorization


trait Factorable{

  /**
   * Returns a single prime factor of N.
   * @param N Positive <code>BigInt</code> N.
   * @return Prime factor of N.
   */
  def factor(N : BigInt) : BigInt;
}