package combinatorics

import utils.Utils

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Permutations {

  /**
   * Returns all the permutations of <i>N</i> integers as a
   * <code>ListBuffer</code> of arrays. The number of arrays in
   * the buffer will be <i>N!</i>.
   * Only valid for positive values of N
   * @param N Positive integer value
   * @return All permutations of N integers.
   */
  def orderedPermutations(N : Int) : ListBuffer[Array[Int]] = {

    var permList = ListBuffer[Array[Int]]()
    val l = List[Int](N)
    val perm : ArrayBuffer[Int] = ArrayBuffer(l:_*)

    permList += perm.toArray
    var h = perm.clone()

    var ctr = 0
    while(ctr < Utils.factorial32(N) - 1){
      h = getNextPermutation(h)
      permList += h.toArray
    }
    return permList
  }


  /**
   * Returns the next permutation in lexicographic order, of the given
   * permutation. <br>
   *   e.g. if <i>perm</i> is <i>[0,1,2,3]</i>, <br>
   *    this function will return an <code>ArrayBuffer</code> <i>[0,1,3,2]</i>.
   * @param perm permutation
   * @return the next permutation.
   */
  def getNextPermutation(perm : ArrayBuffer[Int]) : ArrayBuffer[Int] = {
    val len = perm.length
    val h = perm.clone()
    var i = len - 2
    var done = false
    while(h(i+1) < h(i) && !done){
      i -= 1
      if(i == 0)
        done = true
    }


    var j = len - 1
    while(h(j) < h(i)){
      j-= 1
    }
    var tmp = h(j)
    h(j) = h(i)
    h(i) = tmp

    val p : ArrayBuffer[Int] = h

    for(k <- i + 1 to len - 1){
      p(k) = h(len - 1 + i + 1 - k)
    }

    return p
  }


  /**
   * Calculates the rank of the permutation.
   * @param perm given permutation, as an ArrayBuffer
   * @return rank
   */
  def calculateRank(perm : ArrayBuffer[Int]) = {
    var p = perm.clone()
    val len = p.length
    var r = 0
    for(j <- 0 to len - 1){
      r = r + p(j) * Utils.factorial32(len - j - 1).toInt
      for(i <- j + 1 to len - 1){
        if(p(i) > p(j)){
          p(i) = p(i) - 1
        }
      }

    }
    r
  }

  /**
   * Calculates the unrank of the given rank, in the permutation set
   * of size n. <br>
   *   e.g if rank = 0, and n = 4 <br>
   *   unrank is (0,1,2,3)
   * @param rank rank of permutation
   * @param n number of letters in permutation set
   * @return unrank, the permutation of the given rank.
   */
  def calculateUnrank(rank : Int, n : Int) = {
    if(rank < 0 || rank > Utils.factorial32(n)){

    }
    var arr = ArrayBuffer.fill(n){0}
    var r : Int = rank


    for( j <- 0 to n - 2){
      val d = ((r % Utils.factorial32(j + 2)) / Utils.factorial32(j + 1)).toInt
      r = r - d * Utils.factorial32(j).toInt
      arr(n - j - 2) = d
      for(i <- n - j - 1 to n - 1){
        if(arr(i) > d - 1){
          arr(i) = arr(i) + 1
        }
      }
    }
    arr
  }

  /**
   * Calculates the <i>parity</i> of a permutation.
   * @param permutation
   * @return
   */
  def parity(permutation : ArrayBuffer[Int]) : Int = {
    val len = permutation.length
    var pclone = permutation.clone()
    for(i <- 0 until len){
      pclone(i) = 0
    }
    var k = 0
    for(j <- 0 until len){
      if(pclone(j) == 0){
        k += 1
        pclone(j) = 1
        var i = j
        while(permutation(i) != j){
          i = permutation(i)
          pclone(i) = 1
        }
      }
    }
    (len - k) % 2
  }

  /**
   *
   * @param n
   */
  def generateCyclicPermutationSets(n : Int) : ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]] = {
    var id = new ArrayBuffer[Int]()
    for(i <-Range(0,n)){
      id += i
    }
    var k = n - 1
    var copy = id.clone()
    var cyclicSets = new ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]]()
    while(k > 0){
      var permSet = new ArrayBuffer[ArrayBuffer[Int]]()
      do{
        permSet += copy
        k = n - 1
        copy = shiftLeft(copy, k, 1)
      }
      while(copy(k) != id(k));

      cyclicSets += permSet

      while(copy(k) == id(k) & k > 0){
        k -= 1
        copy = shiftLeft(copy, k, 1)
      }
    }
    cyclicSets
  }


  /**
   *
   * @param array
   * @param k
   * @param n
   * @return
   */
  private def shiftLeft(array : ArrayBuffer[Int], k : Int, n : Int) : ArrayBuffer[Int] = {
    var arr = array.clone()
    var m = n
    while(m>0){
      var tmp = arr(0)
      for(i <-Range(0,k)){
        arr(i) = arr(i+1)
      }
      arr(k) = tmp
      m-=1
    }
    return arr
  }
}
