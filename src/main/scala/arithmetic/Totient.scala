package arithmetic

import factorization.{PollardRho, Factorizer}


object Totient {

  def totient(n : BigInt) = {
    if(n <= 1) 1
    else{
      val factors = Factorizer.factor(n, PollardRho)
      var map =  scala.collection.mutable.Map[BigInt, Int]()

      factors.foreach((f : BigInt) =>
        if(map.contains(f)){
          map.put (f, map(f) + 1)
        }
        else{
          map.put(f, 1)
        }
      )
      var p : BigInt = 1
      for((k,v) <- map){
        p *= k.pow(v) - k.pow(v - 1)
      }

      p
    }
  }


  def sigma(s : Int, n : Int) ={

  }

}
