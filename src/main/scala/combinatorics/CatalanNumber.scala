package combinatorics


class CatalanNumber(idx : Int) {

  val index = idx
  val number = calculateCatalanNumber(index)

  /**
   *
   * @param n
   * @return
   */
  private def calculateCatalanNumber(n : Int ) = {
    if(n < 0)
      None
    else{
      val com : Long = Binomial.coefficient(2 * n, n)
      Some(com / (n + 1))
    }
  }

  def M(x : Int, y : Int) = {
    if( x < 0 || x > 2 * index) None
    else if(y < 0) None
    else if(x + y > 2 * index) 0
    else if((x + y) % 2 == 1)  0
    else {
      val f = Binomial.coefficient(2 * index - x, index - (x + y) / 2)
      val s = Binomial.coefficient(2 * index - x, index - 1 - (x + y) / 2)
      (f - s).toInt
    }
  }


  /**
   *
   * @param arr
   * @return
   */
  def catalanRank(arr : Array[Boolean]): Option[Int] = {
    if (arr.length != 2 * index) return None
    else{
      var f = 0
      var lower = 0
      for(i <- 0 to 2 * index - 2){
        if(arr(i) == false){
          f += 1
        }
        else{
           M(i + 1, f + 1) match{
             case  num : Int =>
               lower +=  num
             case _ =>
               return None
           }
           f -= 1
        }
      }
      return Some(lower)
    }
  }


}
