package arithmetic


class EllipticCurveException(problemInt : BigInt) extends ArithmeticException {
  val exceptionPoint = problemInt
}
