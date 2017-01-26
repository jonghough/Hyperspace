package combinatorics


object BellNumber {

  /**
   * Generates the nth Bell Number, where n = index. The Bell number
   * represents the number of ways to partition a set into subsets.
   * @param index index for bell number
   * @return the index-th bell number.
   */
  def generateBellNumber(index : Int) : BigInt = {
    if(index < 0) throw new IllegalArgumentException("Argument must be non-negative")

    if(index <= 1) 1
    else{
      var sum : BigInt = 0
      for(i <- Range(0, index)){
        val pbn = generateBellNumber(i)
        val bc = Binomial.coefficient(index - 1, i)
        sum += bc * pbn
      }
      sum
    }
  }
}
