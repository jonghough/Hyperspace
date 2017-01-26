package transcendence

/**
 * Beta function
 */
object Beta {

  def beta(x : BigDecimal, y : BigDecimal) : BigDecimal = {
    if(x <= 0 || y <= 0) throw new IllegalArgumentException("Arguments must be positive")
    Gamma.gamma(x) * Gamma.gamma(y) / (Gamma.gamma(x + y))
  }
}
