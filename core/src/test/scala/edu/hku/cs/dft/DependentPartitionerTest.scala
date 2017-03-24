package edu.hku.cs.dft

import edu.hku.cs.dft.traffic.DependentPartitioner

/**
  * Created by jianyu on 3/24/17.
  */
object DependentPartitionerTest {

  def main(args: Array[String]): Unit = {
    val p = new DependentPartitioner(3, Set(2, 1, 4))
    val m1 = ((1, 2, 3), 4)
    val m2 = ((1, 2, 1), 4)
    val m3 = ((2, 2, 3), 4)
    assert(p.getPartition(m1) == p.getPartition(m2))
    assert(p.getPartition(m2) != p.getPartition(m3))
  }

}
