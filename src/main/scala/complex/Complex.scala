package complex


class Complex( val real : Double, val imaginary : Double){

  def magnitude = {
    Math.sqrt(real * real + imaginary * imaginary)
  }


  def argument = {
    Math.atan2(imaginary, real)
  }


  /**
    * conjugate
    */
  def ~ = {
    new Complex(real, -imaginary)
  }

  def +(other : Complex) : Complex = {
    new Complex(real + other.real, imaginary + other.imaginary)
  }


  def -(other : Complex) : Complex = {
    new Complex(real - other.real, imaginary - other.imaginary)
  }

  def *(other : Complex) : Complex = {
    new Complex(real * other.real - imaginary * other.imaginary, imaginary  * other.real + other.imaginary * real)
  }

  def /(other : Complex) : Complex = {
    val numerator = this * other.~
    val denominator = other * other.~
    new Complex(numerator.real / denominator.real, numerator.imaginary / denominator.real)
  }

  def % : Complex = {
    Complex.ONE / this
  }

  def ^(n : Int) : Complex = {
    n match {
      case 0 => Complex.ONE
      case _ => {
        val nextR = Math.pow(magnitude,n)
        val nextArg = argument * n
        Complex.fromPolar(nextR, nextArg)
      }
    }
  }
}

object Complex{
  val ONE = new Complex(1,0)
  val I = new Complex(0,1)
  val ZERO = new Complex(0,0)
  def fromPolar(magnitude : Double, argument : Double) = {
    new Complex(magnitude * Math.cos(argument), magnitude * math.sin(argument))
  }
}
