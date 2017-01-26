package transcendence

import utils.{Utils, Exp}


object Gamma {

  val constants : Seq[BigDecimal] = Seq(
    BigDecimal("676.5203681218851"),
    BigDecimal("-1259.1392167224028"),
    BigDecimal("771.32342877765313"),
    BigDecimal("-176.61502916214059"),
    BigDecimal("12.507343278686905"),
    BigDecimal("-0.13857109526572012"),
    BigDecimal("9.9843695780195716e-6"),
    BigDecimal("1.5056327351493116e-7"))


  def lanczosApproximation(d : BigDecimal) : BigDecimal = {
    if(d < 0.5){
      return BigDecimal(Math.PI) / (Utils.bigSin(d * Math.PI) * lanczosApproximation(1 - d))
    }else{
      val D = d - 1
      var a = BigDecimal("0.99999999999980993")
      val t = D + 7.5

      for(i <- Range(0,constants.size)){
        a += constants(i) / (D + (i + 1))
      }

      return Utils.sqrt(2 * Math.PI) * Exp.bigExp(t, D + 0.5) * Exp.bigExp(Math.E, -1 * t) * a
    }
  }



  def gamma(D : BigDecimal) : BigDecimal = {
    if(D < 0.5){
      Math.PI / (Utils.bigSin(Math.PI) * D * gamma(1 - D))
    }
    else if(D < 1){
      return lanczosApproximation(D)
    }
    else if(D == 1){
      return 1
    }
    else{
      var f = D - 1
      var g = f
      f -= 1
      while(f > 1){
        g *= f
        f -= 1
      }
      f += 1
      return lanczosApproximation(f) * g
    }
  }


  /**
   * COmputes the log Gamma function (logarithm of the gamma function).
   * Method taken from Numerical Recipes
   * @param D
   * @return
   */
  def logGamma(D : BigDecimal) : BigDecimal = {
    val coeffs : List[BigDecimal] = List[BigDecimal](BigDecimal(76.18009172947146),BigDecimal(-86.50532032941677),
      BigDecimal(24.01409824083091),BigDecimal(-1.231739572450155),
      BigDecimal(0.1208650973866179E-2),BigDecimal(-0.5395239384953E-5))

    var y = D
    var x = D
    var tmp = x + 5.5
    tmp -=(x + 0.5) *Exp.ln(tmp,25)
    var ser : BigDecimal = 1.000000000190015
    for(j<-Range(0,coeffs.size)){
      y += 1
      ser += coeffs(j) / (y)
    }
    -tmp + Exp.ln(2.5066282746310005*ser/x, 25)
  }
}


