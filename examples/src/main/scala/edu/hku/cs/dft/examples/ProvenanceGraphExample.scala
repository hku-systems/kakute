package edu.hku.cs.dft.examples

import org.apache.spark.sql.SparkSession

/**
  * Created by jianyu on 4/21/17.
  */
object ProvenanceGraphExample {

  case class Tracer(id: Int, index: Int)

  def main(args: Array[String]): Unit = {
    val spark = SparkSession
      .builder()
      .appName("Provenance Example")
      .config("spark.dft.tracking.mode", "full")
      .getOrCreate()

    val file = if (args.length > 0) args(0) else throw new IllegalArgumentException("no input file")

    val text = spark.read.textFile(file).rdd
    val edges = text.map(t => {
      val s_arr = t.split("\\s+")
      (s_arr(0).toInt, s_arr(1).toInt, s_arr(2).toInt)
    })
    val nodes = edges.flatMap(edge => Array(edge._2, edge._3)).distinct()

    val tainted_edge = edges.taint(t => {
      (1 ,Tracer(t._1, 1), Tracer(t._1, 2))
    }).map(t => (t._2, t._3))

    // Init the node with its own id
    var label_node = nodes.map(t => (t, t))

    for (i <- 1 to 10) {
      // group nodes with the same id
      val new_label = label_node.join(tainted_edge).map(t => (t._2._2, t._2._1)).union(label_node)
      label_node = new_label.reduceByKey(math.min)
    }
    label_node.collectWithTaint().foreach(println)

    spark.stop()
  }
}
