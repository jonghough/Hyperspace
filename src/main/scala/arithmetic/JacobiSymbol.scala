package arithmetic


/**
 *
 */
object JacobiSymbol {

  /**
   *
   * @param numerator
   * @param denominator
   * @return
   */
  def calculate(numerator : BigInt, denominator : BigInt): Int = {
    var jacobi = 1
    var num = numerator
    var den = denominator

		if(numerator == 0) return 0;

		if(numerator < 0){
      num = -1 * num
      if(den % 4 == 3)
        jacobi *= -1

    }

    if(num.gcd(den) != 1){
      return 0;
    }

    while(num != 0){
      if(num == 1) {
        return jacobi
      }
      while(num % 2 == 0) {
				num /= 2

				if (den % 8 == 3 || den % 8 == 5) {
					jacobi *= -1
				}
			}

			if(num % 4 == 3 && den % 4 == 3){
				jacobi *= -1
			}

			val tmp = den % num
			den = num
			num = tmp

    }

    return jacobi
  }

}
