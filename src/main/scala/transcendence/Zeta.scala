package transcendence


class Zeta() {

  val TWOPI : Double = 2 * Math.PI
  var imaginary = 0.0
  var N : Int = 0
  var p = 0.0

  private def init(ip : Double) = {
    imaginary = ip
    N = Math.sqrt(imaginary / TWOPI).toInt
    p = Math.sqrt(imaginary / TWOPI) - N
  }

  private def theta(value : Double) = {
    if(value == 0) throw new IllegalArgumentException("argument must be positive.")
    val finalVal = (value * 0.5 * Math.log(value / TWOPI) - value / 2 - Math.PI / 8 +
      1.0 / (48 * value) + 7.0 / (5760 * Math.pow(value, 3)))
    finalVal
  }

  private def summation(value : Double, max : Int) = {
    var f = 0.0
    val thetaV = theta(value)
    for(i<-Range(0,max)){
      f += 2 * Math.pow(i+1, -0.5) * math.cos(thetaV - value * Math.log(i+1))
    }
    f
  }

  private def remainderTerms = {
    var v = 0.0
    var mv = 0.0
    for(i <- Range(0, Zeta.C0.size)){
      mv += Zeta.C0(i) * math.pow(1 - 2 * p, 2 * i)
    }
    v += mv
    mv = 0
    for(i <- Range(0, Zeta.C1.size)){
      mv += Zeta.C1(i) * Math.pow(1 - 2*p, 2 * i + 1)
    }
    mv *= Math.pow(imaginary * 0.5 / Math.PI, - 0.5)
    v += mv
    mv = 0
    for(i <- Range(0, Zeta.C2.size)){
      mv += Zeta.C2(i) * Math.pow(1 - 2*p, 2 * i)
    }
    mv *= Math.pow(imaginary * 0.5 / Math.PI, -1)
    v += mv

    for(i <- Range(0, Zeta.C3.size)){
      v += Zeta.C3(i) * Math.pow(1 - 2*p, 2 * i + 1)
    }

    for(i <- Range(0, Zeta.C4.size)){
      v += Zeta.C4(i) * Math.pow(1 - 2*p, 2 * i)
    }
    v *= Math.pow(-1, N - 1) * math.pow(imaginary * 0.5 / Math.PI, -0.25)
    v
  }

  def z(ip : Double) = {
    if(ip <= 0) throw new IllegalArgumentException("Argument must be positive.")
    this.init(ip)
    val s = summation(imaginary, N)
    s + remainderTerms
  }

}


object Zeta{

    val C0 : List[Double] = List[Double](0.38268343236508977173, 0.43724046807752044936, 0.13237657548034352333,
      -0.01360502604767418865, -0.01356762197010358088, -0.00162372532314446528, 0.00029705353733379691,
      0.00007943300879521469, 0.00000046556124614504, -0.00000143272516309551, -0.00000010354847112314,
      0.00000001235792708384, 0.00000000178810838577, -0.00000000003391414393, -0.00000000001632663392)

    val C1 : List[Double] = List[Double](0.02682510262837535, -0.01378477342635185, -0.03849125048223508,
      -0.00987106629906208, 0.00331075976085840, 0.00146478085779542, 0.00001320794062488,
      -0.00005922748701847, -0.00000598024258537, 0.00000096413224562, 0.00000018334733722)

    val C2 : List[Double] = List[Double](0.005188542830293, 0.000309465838807, -0.011335941078299, 0.002233045741958,
      0.005196637408862, 0.000343991440762, -0.000591064842747, -0.000102299725479, 0.000020888392217,
      0.000005927665493, -0.000000164238384, -0.000000151611998)

    val C3 : List[Double] = List[Double]( 0.0013397160907, -0.0037442151364, 0.0013303178920, 0.0022654660765,
      -0.0009548499998, -0.0006010038459, 0.0001012885828, 0.0000686573345, -0.0000005985366,
      -0.0000033316599, -0.0000002191929, 0.0000000789089, 0.0000000094147 )

    val C4 : List[Double] = List[Double](0.00046483389, -0.00100566074, 0.00024044856, 0.00102830861,
      -0.00076578609, -0.00020365286, 0.00023212290, 0.00003260215, -0.00002557905, -0.00000410746,
      0.00000117812, 0.00000024456 )
}
