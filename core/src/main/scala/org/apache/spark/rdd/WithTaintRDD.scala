package org.apache.spark.rdd

import edu.hku.cs.dft.tracker.SelectiveTainter
import org.apache.spark.{Partition, TaskContext}

import scala.reflect.ClassTag

/**
  * A [[WithTaintRDD]] has the data along with tag in corresponding position
  * But the problem is how to design the tag ???
  */

class WithTaintRDD[T: ClassTag](
  val prev: RDD[T]) extends RDD[(T, Map[Int, Int])](prev){
  /**
    * :: DeveloperApi ::
    * Implemented by subclasses to compute a given partition.
    */
  override def compute(split: Partition, context: TaskContext): Iterator[(T, Map[Int, Int])] = {
    val selectiveTainter: SelectiveTainter = new SelectiveTainter(Map(), 0)
    firstParent[T].iterator(split, context).map(d => {
      (d, selectiveTainter.getTaintList(d))
    })
  }

  /**
    * Implemented by subclasses to return the set of partitions in this RDD. This method will only
    * be called once, so it is safe to implement a time-consuming computation in it.
    *
    * The partitions in this array must satisfy the following property:
    * `rdd.partitions.zipWithIndex.forall { case (partition, index) => partition.index == index }`
    */
  override protected def getPartitions: Array[Partition] = this.firstParent.partitions
}
