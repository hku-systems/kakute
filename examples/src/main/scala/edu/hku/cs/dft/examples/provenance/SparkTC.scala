package edu.hku.cs.dft.examples.provenance

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.SparkSession

import scala.collection.mutable
import scala.util.Random

/**
  * Created by jianyu on 5/10/17.
  */
object SparkTC {

  def main(args: Array[String]) {
    val spark = new SparkContext(new SparkConf())

    val file = args(0)

    val partition = args(1).toInt

    var tc = spark.textFile(file)
      .map(t => {
        val splits = t.split("\\s+")
        (splits(0), splits(1))
      })

    // Linear transitive closure: each round grows paths by one edge,
    // by joining the graph's edges with the already-discovered paths.
    // e.g. join the path (y, z) from the TC with the edge (x, y) from
    // the graph to obtain the path (x, z).

    // Because join() joins on keys, the edges are stored in reversed order.
    val edges = tc.map(x => (x._2, x._1))

    // This join is iterated until a fixed point is reached.
    var oldCount = 0L
    var nextCount = tc.count()
    do {
      oldCount = nextCount
      // Perform the join, obtaining an RDD of (y, (z, x)) pairs,
      // then project the result to obtain the new (x, z) paths.
      tc = tc.union(tc.join(edges).map(x => (x._2._2, x._2._1))).distinct().cache()
      nextCount = tc.count()
    } while (nextCount != oldCount)

    println("TC has " + tc.count() + " edges.")
    spark.stop()
  }
}
