import combinatorics.Partitions
import org.scalatest.FlatSpec


class PartitionsTest extends FlatSpec{

  "Size of Partition set of 5" should "be 7" in {
    val p5 = Partitions.generatePartitions(5).size
    assert(p5 == 7)
  }

  "Size of Partition set of 10" should "be 42" in {
    val p10 = Partitions.generatePartitions(10).size
    assert(p10 == 42)
  }

  "Size of Partition set of 12" should "be 77" in {
    val p12 = Partitions.generatePartitions(12).size
    assert(p12 == 77)
  }

  "Size of Partition set of 40" should "be 37338" in{
    val p40 = Partitions.generatePartitions(40).size
    assert(p40 == 37338)
  }

  "Estimate number of partitions of 100 " should "be 190568944" in {
    val est = Partitions.calculateHREstimate(100)
    assert((est - 190568944).toBigInt() == 0)
  }

//  "EConjugate partition " should "be 190568944" in {
//    val est = Partitions.calculateHREstimate(100)
//    assert((est - 190568944).toBigInt() == 0)
//  }
}
