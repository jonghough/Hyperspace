package utils


object Utils {

  val PI = BigDecimal(Math.PI)

  def sqrt(N : BigDecimal) : BigDecimal = {
    if(N <= 0)
      return 0;
    else{
      var tmp : BigDecimal = 1
      val n : BigDecimal = N
      var root : BigDecimal = n+ (n / tmp)
      val epsilon : BigDecimal = BigDecimal(0.0001)

      while((root - tmp).abs > epsilon){
        tmp = root;
        root = (tmp + (n / tmp)) / 2
      }
      return root;
    }

  }

  def cbrt(N : BigDecimal) : BigDecimal = {
    if(N <= 0)
      return 0;
    else{
      var third : BigDecimal = BigDecimal("0.3333333")
      var tmp : BigDecimal = 1
      val n : BigDecimal = N
      var root : BigDecimal = n+ (n / tmp)
      val epsilon : BigDecimal = BigDecimal(0.0001)

      while((root - tmp).abs > epsilon){
        tmp = root
        root = third * ( 2 * tmp + (n / (tmp * tmp)))
      }
      return root;
    }
  }

  def nroot(N : BigInt, root : Int) : BigDecimal = {
    if(N <= 0)
      return 0;
    else{

      var r : BigDecimal = BigDecimal(root.toString())
      var inv : BigDecimal = BigDecimal(1.0) / r
      var tmp : BigDecimal = 1;
      val n : BigDecimal = BigDecimal(N)
      val rmo = r - 1
      var nthRoot : BigDecimal = r * ( rmo * tmp + (n / (tmp.pow(root - 1))))
      val epsilon : BigDecimal = BigDecimal(0.0001);

      while((root - tmp).abs > epsilon){
        tmp = root;
        nthRoot = r * ( rmo * tmp + (n / (tmp.pow(root - 1))))

      }
      return nthRoot;
    }
  }



  def nroot(n : BigDecimal, root : Int) : BigDecimal = {
    val inv : Float = 1.0f / root
    val r = BigDecimal(inv)
    var temp = r * n
    var rmo = BigDecimal(root - 1)
    var nthRoot = r * ((rmo * temp) + n / temp.pow(root - 1))
    while((nthRoot - temp).abs > 0.0001){
      temp = nthRoot
      nthRoot = r * ((rmo * temp) + n / temp.pow(root - 1))
    }
    return nthRoot
  }



  def factorial32(n : Int) : Long = {
    if(n < 0) 0
    else if(n <= 1) 1
    else {
      val l = List.range(1, n + 1)
      l.reduce(_ * _)

    }

  }


  def bigSin(D : BigDecimal) = {
    if(D.abs <= 2 * PI){
      BigDecimal(Math.sin(D.asInstanceOf[Double]))
    }
    else{
      var f = D / (2 * PI)
      val F : BigInt = f.toBigInt()
      f = (f - BigDecimal(F)) * (2 *PI)
      BigDecimal(Math.sin(f.asInstanceOf[Double]))
    }
  }

  def bigCos(D : BigDecimal) = {
    if(D.abs <= 2 * PI){
      BigDecimal(Math.cos(D.asInstanceOf[Double]))
    }
    else{
      var f = D / (2 * PI)
      val F : BigInt = f.toBigInt()
      f = (f - BigDecimal(F)) * (2 *PI)
      BigDecimal(Math.cos(f.asInstanceOf[Double]))
    }
  }

  def bigTan(D : BigDecimal) = {
    val s = bigSin(D)
    val c = bigCos(D)
    try{
      s / c
    }
    catch{
      case e : ArithmeticException =>
      throw e;
    }
  }
}
