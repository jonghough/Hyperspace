package arithmetic

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object BernoulliNumbers {

  /**
   *
   * @param n
   * @return
   */
  def generateBernoulliNumbers(n : Int) : ListBuffer[Rational] = {
    var initialArray = ArrayBuffer.fill(n + 2){0}
    initialArray(1) = 1

    var bernoulliList = ListBuffer.empty[Rational]
    bernoulliList += new Rational(1,1)
    var flag = true

    var a = 1
    var b = 1
    var c = -2
    var d = 1

    for(i <- 0 until 2 * n - 2){
      if (flag) {
        a += 1
        b = 4 * b
        c = -c
        d = c * (b - 1)
        for( k <- Range(a,0,-1)) {
          initialArray(k) += initialArray(k + 1)
        }
      }
      else {
        for(k <- Range(1,a)){
          initialArray(k) += initialArray(k - 1)
        }
        bernoulliList += new Rational( initialArray(a - 1), d)

      }
      flag = !flag
    }
    return bernoulliList
  }

}
