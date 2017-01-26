package combinatorics

import scala.collection.mutable

object StirlingNumbers {

  /**
    *
    * @param m
    * @param n
    * @param signed
    * @return
    */
  def generateFirstKind(m : Int, n : Int, signed : Boolean) : Long = {
    val stirlingMap = new mutable.HashMap[Int,Long]
    stirlingMap += hash(0,0) -> 1
    for(i <- Range(0,m+1)){
      if(i == 0){
        stirlingMap += ((hash(0,1), 0L))
      }
      else{
        stirlingMap += ((hash(i,0) , 0L))
        stirlingMap += ((hash(i, i + 1), 0L))
      }
    }

    if(!signed){
      for(i <- Range(1,m+1)){
        for(j <- Range(1,Math.min(i, n)+1)){
          val item : Long = stirlingMap(hash(i - 1, j))
          val f = j  * item
          val g = stirlingMap(hash(i - 1, j - 1))
          stirlingMap(hash(i,j)) = g + f
        }
      }
    }
    else {
      for (i <-  Range(1,m+1)) {
        for (j <- Range(1,Math.min(i, n)+1)) {
          val item: Long = stirlingMap(hash(i - 1, j))
          val f = (i - 1) * item
          val g = stirlingMap(hash(i - 1, j - 1))
          stirlingMap.update((hash(i, j)), g - f)
        }
      }
    }

    return stirlingMap(hash(m,n))
  }

  /**
   *
   * @param a
   * @param b
   * @return
   */
  def hash (a : Int, b : Int) : Int = {
    val i = a * 31627
    val j = (b << 1) * 1299721
    val k = ( a << 2) | (b >> 3)
    val l = a ^ (b >> 2)
    i + 13 * (j - k) + b * 233 - a * 7 + l *23
  }
}
