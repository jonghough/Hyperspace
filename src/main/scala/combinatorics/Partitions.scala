package combinatorics

import utils.{Exp, Utils}

import scala.collection.mutable.ArrayBuffer


object Partitions {

  def generatePartitions(N : Int) : ArrayBuffer[ArrayBuffer[Int]]= {
    generatePartitions(N, 1)

  }

  private def generatePartitions(N : Int, min : Int) : ArrayBuffer[ArrayBuffer[Int]] = {
    var arr : ArrayBuffer[ArrayBuffer[Int]] = new ArrayBuffer[ArrayBuffer[Int]]()
    var a = new ArrayBuffer[Int]()

    a += N
    arr += a
    if(N == 1) return arr

    var i = min
    while(i <= N - i){
      var sub = generatePartitions(N - i, i)
      for(al <- sub){
        al += i
      }
      i += 1
      arr ++= sub
    }
    arr
  }

  def conjugatePartition(partition : List[Int]) : List[Int] = {
    val l = partition(0)
    var init = new ArrayBuffer[Int]()
    for(i <- Range(0,l+1)){
      init += (1)

    }

    var np = partition(0)
    for(j <- Range(0,partition.size)){
      for(i <- Range(0,partition(j))){
        if(i < init.size) {
          init.insert(i, init(i) + 1)
        }
        else init+=1
      }
    }
    var res = new ArrayBuffer[Int]()
    res ++= init.slice(0, np)
    res.toList
  }

  /**
   *
   * @param n
   * @return
   */
  def calculateHREstimate(n : BigInt) = {
    println("hr n is "+n)
    if(n <= 0) throw new IllegalArgumentException("Argument must be positive.")
    else{
      val nd : BigDecimal = BigDecimal(n) - (1.0 / 24.0)
      val rt : BigDecimal = Utils.sqrt(nd * 2.0 / 3.0)
      val ex : BigDecimal = Math.PI * rt
      val in : BigDecimal = 1.0 / ex
      val ml : BigDecimal = 1.0 / (Utils.sqrt(3.0) * 4.0 * nd)
      Exp.bigExp(Math.E, ex) * ml * (1 - in)
    }
  }
}
