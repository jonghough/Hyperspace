package arithmetic

/**
  * Curve points for <i>Elliptic Curves</i>.
  * Can either be an (x,y) pair, or the point
  * at <i>infinity</i>.
  */
abstract class CurvePoint
case class Point(a: BigInt, b: BigInt) extends CurvePoint
case class Infinity() extends CurvePoint

