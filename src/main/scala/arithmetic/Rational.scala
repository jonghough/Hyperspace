package arithmetic


/**
 * arithmetic.Rational numbers class.
 * @param numerator
 * @param denominator
 */
class Rational(numerator : Long, denominator : Long) {

  val divisor = gcd(numerator, denominator)
  val _numerator = numerator / divisor
  val _denominator = denominator / divisor

  override def toString = _numerator +", "+_denominator

  /**
   *
   * @param other
   * @return
   */
  def +(other : Rational) : Rational = {
    val num = this._numerator * other._denominator + this._denominator * other._numerator
    val den = this._denominator * this._denominator
    new Rational(num, den)
  }

  /**
   *
   * @param other
   * @return
   */
  def -(other : Rational) : Rational = {
    val num = this._numerator * other._denominator - this._denominator * other._numerator
    val den = this._denominator * this._denominator
    new Rational(num, den)
  }

  /**
   *
   * @param other
   * @return
   */
  def *(other : Rational) : Rational = {
    val num = this._numerator * other._numerator
    val den = this._denominator * this._denominator
    new Rational(num, den)
  }

  /**
   *
   * @param other
   * @return
   */
  def /(other : Rational) : Rational = {
    val num = this._numerator / other._numerator
    val den = this._denominator / this._denominator
    new Rational(num, den)
  }

  /**
   *
   * @return
   */
  def %() : Rational = {
    if (this._numerator == 0)
      throw new Exception("Cannot invert zero")
    else {
      val num = this._denominator
      val den = this._numerator
      new Rational(num, den)
    }
  }

  /**
   *
   * @param a
   * @param b
   * @return
   */
  def gcd(a : Long, b : Long) : Long = {
    if(b == 0)
      return a
    else{
      return gcd(b, (b + a) % b)
    }

  }

}
