package factorization

import utils.Utils

import scala.util.Random

/**
  * Large integer factorization using Pollard Rho
  * algorithm.
  */
object PollardPMO extends Factorable{

  override def factor(N : BigInt) : BigInt = {
    if(N.mod(2) == 0){
      return 2;
    }
    else if(N.mod(3) == 0){
      return 3;
    }
    else if(N.mod(5) == 0){
      return 5;
    }
    else if(N.mod(7) == 0){
      return 7;
    }
    else{
      val p5 = BigInt(120);
      val p10 = BigInt(3628800);
      val p15 = BigInt(1307674368000L);
      val p20 = BigInt("2432902008176640000");
      val p25 = BigInt("15511210043330983907819520");
      val p35 = BigInt("10333147966386144222209170348167175077888");

      val rand: Random = Random;


      var bound : BigInt = Utils.sqrt(BigDecimal(N)).toBigInt();

      if(bound < p35 && bound > p25) {
        bound = p35;
      }
      else if(bound > p25) {
        bound = p25;
      }
      else if(bound > p20) {
        bound = p20;
      }
      else if(bound > p15) {
        bound = p15;
      }
      else if(bound > p10) {
        bound = p10;
      }
      else if(bound > p5) {
        bound = p5;
      }

      var count : Int = 1000000;
      while(count > 0){
        val a : BigInt = BigInt(N.bitLength, rand);
        val g : BigInt = a.modPow(bound, N);
        val f : BigInt = g.gcd(N);
        if(f > 1 && f < N){
          if(f.isProbablePrime(100)){
            return f;
          }
          else{
            return PollardPMO.factor(f);
          }
        }
      }

      return N;

    }
  }
}
