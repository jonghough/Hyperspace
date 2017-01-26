package arithmetic

import java.util.Random

/**
 *
 */
class EllipticCurve(A : BigInt, B : BigInt, N : BigInt) {


  /**
   *
   * @param point1
   * @param point2
   * @return
   */
  def add(point1: CurvePoint, point2: CurvePoint): CurvePoint = (point1, point2) match {
    case (Point(a, b), Point(c, d)) => sum(Point(a, b), Point(c, d))
    case (Point(a, b), Infinity()) => return point1
    case (Infinity(), Point(_, _)) => return point2
    case (_, _) => Infinity()

  }



  /**
   *
   * @param p1
   * @param p2
   * @return
   */
  def sum(p1: Point, p2: Point): CurvePoint = {
    if (p1.a == p1.a && p1.b == -1 * p2.b) {
      return new Point(BigInt("0"), BigInt("0"))

    }
    else {
      var lambda = BigInt("0")
      if (p1.a == p2.a && p1.b == p2.b) {
        val denom = (2 * p1.b + N) % N
        try {
          var denomInv = denom.modInverse(N)
          lambda = (((2 * p1.a * p1.a + this.A) * denomInv) + N) % N

        } catch {
          case e: ArithmeticException =>{
            System.out.println(">>>>>>>>>>>>>>>>>>>   exception adding "+lambda.toString())
            throw new EllipticCurveException(denom);
          }
        }

      }
      else {
        lambda = ((p2.b - p1.b) + this.N) % this.N
        try {
          var inv = p2.b.modInverse(this.N)
          inv = (inv + this.N) % this.N
          lambda = lambda * inv
        } catch {
          case e: ArithmeticException => throw new EllipticCurveException(p2.b);
        }
      }
      lambda = (lambda + this.N) % this.N
      var nextx = ((lambda * lambda - p1.a - p2.a) + this.N) % this.N
      if (nextx < 0) {
        nextx = (nextx + this.N) % this.N
      }
      var nexty = ((-1 * lambda * nextx - p1.b - (lambda * p1.a)) + this.N) % this.N
      if (nexty < 0) {
        nexty = (nexty + this.N) % this.N
      }
      return Point(nextx, nexty)
    }

  }

  /**
   * Function for Double and Add elliptic curve addition.
   * @param p
   * @param d
   * @return
   */
  def doubleAndAdd(p: CurvePoint, d: Int): CurvePoint = {
    var q : CurvePoint = Infinity()
    var r = p
    var D = BigInt(d)
    for (i <- Range(0,D.bitLength)){
      if (((D >> i) & BigInt(1)) == BigInt(1)) {

        //may throw an exception -- possible factor found.

        var s = this.add(q, r)
        q = s
      }
      r = this.add(r, r)
    }
    return q
  }

}


object EllipticCurve {

  def addInc (ellipticCurve: EllipticCurve, p : CurvePoint, q : CurvePoint) : CurvePoint = {
    var r : CurvePoint = null
    try{
      r = ellipticCurve.add(p, q)
    } catch{
      case e : Exception => {}
    }
    return r
  }

  /**
   * Finds a prime factor of the given positive integer.
   * @param n
   * @return
   */
  def factorLenstra(n : BigInt) : BigInt = {
    if(n.isProbablePrime(100)){
      return n;
    }
    var rand = new Random(System.currentTimeMillis())
    var x = BigInt(32, rand)
    var y = BigInt(32, rand)
    var a = BigInt(32, rand)
    var b = (y * y - ((x * x * x) - a * x)) % n

    val bound = 1000L
    var ellipticCurve = new EllipticCurve(a,b,n)

    var point : CurvePoint = new Point(x,y)

    var counter = 2L
    var nextPoint : CurvePoint = Infinity()
    while(counter < bound){
      point match {
        case Point(a, b) =>
        case Infinity() => {
          throw new Exception()

        }
      }
      if(point.isInstanceOf[Infinity]){
        factorLenstra(n)
      }
      else {
        try {
          nextPoint = ellipticCurve.doubleAndAdd(point, counter.asInstanceOf[Int])
        }
        catch {
          case e: EllipticCurveException => {
            if (e.exceptionPoint != BigInt(0)) {
              val l = e.exceptionPoint.gcd(n)
              if (l > 1 && a != n) {
                if (l.isProbablePrime(100)) {
                  return l
                }
                else {
                  return factorLenstra(l)
                }
              }
            }
          }
        }
        point = nextPoint
        counter += 1
      }
    }
    throw new Exception("could not factor")

  }
}
