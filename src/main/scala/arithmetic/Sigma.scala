package arithmetic

import factorization.{PollardRho, Factorizer}

/**
 * Sigma Function
 */
object Sigma {

  def sigma(sub : Int, n : BigInt) : BigInt =
  {
    if(sub < 0) throw new IllegalArgumentException("Cannot have negative subscript.")
    else if(sub == 0){
      sigma0(n)
    }
    else{
      val factorList = Factorizer.factor(n, PollardRho)
      var map =  scala.collection.mutable.Map[BigInt, Int]()

      var num : BigInt = 1
      var den : BigInt = 1
      factorList.foreach((f : BigInt) =>
        if(map.contains(f)){
          map.put (f, map(f) + 1)
        }
        else{
          map.put(f, 1)
        }
      )

      for((k,v) <- map){
        num *= k.pow((v + 1) * sub) - 1
        den *= k.pow(sub) - 1
      }

      num / den
    }
  }

  private def sigma0(n : BigInt) = {
    val factorList = Factorizer.factor(n, PollardRho)
    var map =  scala.collection.mutable.Map[BigInt, Int]()

    factorList.foreach((f : BigInt) =>
      if(map.contains(f)){
        map.put (f, map(f) + 1)
      }
      else{
        map.put(f, 1)
      }
    )

    var p : BigInt = 1
    for((k,v) <- map){
      p *= v + 1
    }

    p
  }
}
