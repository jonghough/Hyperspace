package factorization

/**
 * Implementation of P+1 Factorization method
 */
object PPO extends Factorable{
  /**
   * Returns a single prime factor of N.
   * @param N Positive <code>BigInt</code> N.
   * @return Prime factor of N.
   */
  override def factor(N: BigInt): BigInt = {
    println("begin "+ N)
    if(N.isProbablePrime(100)) return N
    else{
      var attempts = 2
      while(attempts < 1000){
        var m = BigInt(attempts)
        var trials = 2
        var count : BigInt = 2
        while(trials < 100){
          var r = calc(N, m, count)
          r = (r - 2).gcd(N)
          if(r > 1 && r <  N){
            if(r.isProbablePrime(100)){

              println("found fac "+r)
              return r
            }
            else return factor(r)
          }
          trials+=1
          count *= trials
        }
        attempts+=1
      }
      return N
    }
  }


  private def calc(N : BigInt, base : BigInt, m : BigInt) : BigInt = {
    var x = base
    var y = (base * base - 2) % N
    var bytes = m.toByteArray
    var msbi : Int = 0

    var b : Array[Byte] = null

    if((bytes(0) & 0xFF) == 0){
      var b2 : Array[Byte] = Array.fill(bytes.length - 1){0}
      for(i<-Range(1,bytes.length)){
        b2(i-1) = bytes(i)
      }
      b = b2
    }
    else{
      b = bytes;
    }
    for(i <- Range(0, b.length)){
      //first byte
      if(i == 0){
        var j = 8
        var cont = true
        while(j >= 0 && cont){
          if(((b(0) >> j) & 1) == 1){
            msbi = j
            cont = false //break
          }
          j-= 1
        }
        if(msbi > 0){
          for(k <- msbi - 1 to 0 by -1){
            if((b(0) & 1) == 1){
              x = (x * y - base) % N
              y = (y * y - 2) % N
            }
            else{
              y = (x * y - base) % N
              x = (x * x - 2) % N
            }
          }
        }
      }
      else{
        for(k <- 7 to 0 by -1 ){
          if(((b(i) >> k) & 1) == 1){
            x = (x * y - base) % N
            y = (y * y - 2) % N
          }
          else{
            y = (x * y - base) % N
            x = (x * x - 2) % N
          }
        }
      }
    }
    return x
  }
}
