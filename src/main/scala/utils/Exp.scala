package utils


/**
 * Contains a collection of exponential and logarithmic functions for
 * BigInt and BigDecimal types.
 */
object Exp {

  /**
   * Calculates base to the power exponent, where exponent is a
   * BigInt and base is a BigDecimal.
   * @param base base of exponential function
   * @param exponent exponent of exponential function
   * @return base raised to the power exponent
   */
  def exp(base : BigDecimal, exponent : BigInt) : BigDecimal = {
    if( exponent < 0){
      return BigDecimal("1") / exp(base, -1 * exponent)
    }
    else if(exponent == 0) {
      return BigDecimal("1")
    }
    else if(exponent == 1){
      return base
    }
    else{
      if(exponent % 2 == 0){
        return exp(base.pow(2), exponent / 2)
      }
      else{
        return base * exp(base.pow(2), (exponent - 1) / 2)
      }
    }
  }

  /**
   * Calculates base to the power exponent, where exponent is a
   * Double and base is a BigDecimal.
   * @param base base of exponential function
   * @param exponent exponent of exponential function
   * @return base raised to the power exponent
   */
  def bigExp(base : BigDecimal, exponent : Double) : BigDecimal = {
    bigExp(base, BigDecimal(exponent))
  }

  /**
   * Calculates base to the power exponent, where exponent is a
   * BigDecimal and base is a BigDecimal.
   * @param base base of exponential function
   * @param exponent exponent of exponential function
   * @return base raised to the power exponent
   */
  def bigExp(base : BigDecimal, exponent : BigDecimal) : BigDecimal = {
    val intpart = exponent.toBigInt()
    val p1 = exp(base, intpart)
    val mantissa = exponent - BigDecimal(intpart)

    val p2 = taylorExpand(mantissa * ln (base, 100), 100)
    p1 * p2
  }

  /**
   * Approximation of <i>Taylor series</i> expansion of <i>ex</i> about the
   * point, <code>value</code>, calculating the first <code>max</code> terms
   * of the expansion.<br>
   * This is used for points less than 1, where the approximation
   * converges relatively quickly.
   * @param value arithmetic.Point for taylor expansion
   * @param max number of terms to calculate
   * @return taylor expansion approximation
   */
  def taylorExpand(value : BigDecimal, max : Int) : BigDecimal = {
    var runningTotal = BigDecimal("0")
    val powers = List.range(0, max + 1)
    val summands = powers.map(taylorVal(value,_))
    summands.reduce(_+_)
  }

  /**
   * Calculation of individual term in the taylor expansion of <i>ex</i>,
   * where value is the point of expansion, and exponent is the integral
   * power to raise by. <br>
   *   ie. <i>x**n / n!</i>
   * @param value arithmetic.Point for taylor expansion
   * @param exponent Power to raise by
   * @return Term in taylor expansion
   */
  def taylorVal(value : BigDecimal, exponent : Int) : BigDecimal = {
    val power = exp(value, BigInt(exponent))
    power / BigDecimal(factorial(exponent))
  }

  /**
   * Factorial function for BigInt values.
   * Valid only for non-negative integer values.
   * This should only be used for small values of n,
   * well within 32-bit int range.
   * @param n
   * @return n!
   */
  def factorial (n : BigInt) : BigInt = {
    if(n < 0) return 0
    else if(n <= 1) return 1
    else return n * factorial(n - 1)

  }

  /**
   * Calculates the binary logarithm, with a given decimal scale.
   * @param D
   * @param scale
   * @return log2(D)
   */
  def log2(D : BigDecimal, scale : Int) = {
    if( D <= 0)
      throw new IllegalArgumentException("Argument must be positive.")
    var d = D
    var result = BigDecimal(0)
    var mantissa = BigDecimal(0.5)
    //val two = BigDecimal(2)

    while(d < 1 || d >= 2){
      while(d < 1){
        d *= 2
        result -= 1
      }
      while(d >= 2){
        d /= 2
        result += 1
      }
    }

    for(i <- 1 to scale){
      d *= d
      if(d >= 2){
        d /= 2
        result += mantissa
      }
      mantissa /= 2
    }
    result
  }

  /**
   * Calculates the natural logarithm of BigDecimal <code>D</code>.
   * @param D
   * @param scale
   * @return Natural logarithm
   */
  def ln(D : BigDecimal, scale : Int) = {
    if( D <= 0)
      throw new IllegalArgumentException("Argument must be positive.")
    val LOG_2_E = BigDecimal("1.4426950408889634073599246810017822653278448610936510082308932567674020219783415086567401885986328125");

    log2(D, scale) / LOG_2_E
  }
}
