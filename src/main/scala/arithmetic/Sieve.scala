package arithmetic

import scala.collection.mutable.ArrayBuffer


object Sieve {

  /**
    *
    * @param n
    * @return
    */
  def eratosthenes(n: Int): List[Int] = {
    if (n < 1) throw new Exception("argument must be positive")
    else if (n == 1) List.empty
    else {
      var buff = Array.fill(n){true}

      val sqrt = Math.sqrt(n).asInstanceOf[Int] + 1

      for (i <- Range(2, sqrt)) {
        var j = 0
        while (i * i + i * j < n) {
          buff(i * i + i * j) = false
          j += 1
        }

      }

      var res = new ArrayBuffer[Int]()
      for (i <- Range(2, n)) {
        if (buff(i))
          res += i
      }
      res.toList
    }
  }


  /**
    *
    * @param n
    * @return
    */
  def atkin(n: Int): List[Int] = {
      if (n < 1) throw new Exception("argument must be positive")
      else if (n == 1) List.empty
      else {
        var buff = Array.fill(n){false}

        val sqrt = Math.sqrt(n).asInstanceOf[Int] + 1

        for (i <- Range(0, sqrt)) {
          for (j <- Range(0, sqrt)) {
            var z = 4 * i * i + j * j
            if (z < n && (z % 12 == 1 || z % 12 == 5)) {
              buff(z) = !buff(z)
            }
            z = 3 * i * i + j * j
            if (z < n && z % 12 == 7)
              buff(z) = !buff(z)
            z = 3 * i * i - j * j
            if (z < n && i > j && z % 12 == 11) {
              buff(z) = !buff(z)
            }
          }
        }

        var res = new ArrayBuffer[Int]()
        res +=(2, 3)

        for (i <- Range(2, n)) {
          var I: Long = i.asInstanceOf[Long]
          var j = 0
          if (buff(i)) {
            var J: Long = j.asInstanceOf[Long]
            while (I * I + I * J < Int.MaxValue && i * i + i * j < n) {
              buff(i * i + i * j) = false
              j += 1
            }
            res += i
          }
        }
        res.toList
      }
    }




}
