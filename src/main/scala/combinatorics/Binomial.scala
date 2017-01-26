package combinatorics


object Binomial {

  /**
   * Binomial coefficient, equal to the value of the combination (n k).
   * i.e. the number of ways to select k unordered items from a collection of
   * n items, without repetition.
   * Here, k <= n.
   * @param n Number of items
   * @param k Number of items in selection
   * @return Possible number of ways of collection k items from
   *         selection of n items.
   */
  def coefficient(n : Int, k : Int) : Long = {
    if( n < 0 || k < 0)
      return 0
    else if(k > n)
      throw new IllegalArgumentException("Exception: k > n, not allowed.")
    else if(k == 0 || k == n)
      return 1
    else if(k == 1 || k == n - 1)
      return n
    else{
      var r = k
      if(r > n - k)
        r = n - k

      var coeff : Long = 1
      for(i <- Range(1,r+1)){
        coeff = coeff * (n - (r - i))
        if(coeff < 0)
          throw new Exception("Overflow Integer ")
        coeff = coeff / i
      }
      return coeff
    }
  }
}
