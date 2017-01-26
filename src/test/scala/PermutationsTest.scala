import combinatorics.Permutations
import org.scalatest.FlatSpec

import scala.collection.mutable.ArrayBuffer


class PermutationsTest extends FlatSpec {

  "Rank of (1 0 2 3 5 4)" should "be 121" in{
    val rank = Permutations.calculateRank(ArrayBuffer(1, 0, 2, 3, 5, 4))
    assert(rank == 121)
  }

  "Rank of (0 1 2 3 4 5)" should "be 0" in{
    val rank = Permutations.calculateRank(ArrayBuffer(0, 1, 2, 3, 4, 5 ))
    assert(rank == 0)
  }

  "Rank of (3 1 0 2)" should "be 20" in{
    val rank = Permutations.calculateRank(ArrayBuffer(3, 1, 0, 2))

    assert(rank == 20)
  }

  "unrank of 1, for permutaiton set of size 3" should "be (0,2,1)" in{
    val unrank = Permutations.calculateUnrank(1,3)
    assert(unrank(0) == 0)
    assert(unrank(1) == 2)
    assert(unrank(2) == 1)
  }

  "number of cyclic permutation sets on 4 letters" should "be 6" in {
    val ps = Permutations.generateCyclicPermutationSets(4)
    assert(ps.size == 6)
  }

  "number of cyclic permutation sets on 6 letters" should "be 120" in {
    val ps = Permutations.generateCyclicPermutationSets(6)
    assert(ps.size == 120)
  }
}
