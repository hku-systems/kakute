package edu.hku.cs.dft.examples.provenance

import org.apache.spark.sql.SparkSession

/**
  * Created by jianyu on 4/21/17.
  */
object ProvenanceGraphExample {

  def main(args: Array[String]): Unit = {
    val spark = SparkSession
      .builder()
      .appName("Provenance Example")
      .getOrCreate()

    val file = if (args.length > 0) args(0) else throw new IllegalArgumentException("no input file")

    val trace = if (args.length > 1)
      true
    else
      false

    val text = spark.read.textFile(file).rdd
    var edges = text.map(t => {
      val s_arr = t.split("\\s+")
      (s_arr(0).toInt, s_arr(1).toInt)
    })

    if (trace)
      edges = edges.zipWithUniqueId().taint(t => {
        ((t._2, t._2), -1)
      }).map(_._1)

    val nodes = edges.flatMap(edge => Array(edge._1, edge._2)).distinct()

    // Init the node with its own id
    var label_node = nodes.map(t => (t, t))

    for (i <- 1 to 10) {
      // group nodes with the same id
      val new_label = label_node.join(edges).map(t => (t._2._2, t._2._1)).union(label_node)
      label_node = new_label.reduceByKey(math.min)
    }

    if (trace)
      label_node.collectWithTaint().foreach(println)
    else
      label_node.collect().foreach(println)

    readLine()

    spark.stop()
  }
}
