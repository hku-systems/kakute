package edu.hku.cs.dft.examples.provenance

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.SparkSession

import scala.collection.mutable

/**
  * Created by jianyu on 4/21/17.
  */
object ProvenanceGraphExample {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()

    val spark = new SparkContext(conf)

    val file = if (args.length > 1) args(0) else throw new IllegalArgumentException("no input file")

    val partitions = args(1).toInt

    val iteration = args(2).toInt

    val trace = args(3).toBoolean

    val text = spark.textFile(file)
    var edges = text.map(t => {
      val s_arr = t.split("\\s+")
      (s_arr(0), s_arr(1))
    })

    if (trace)
      edges = edges.zipWithUniqueId().taint(t => {
        ((t._2 + 1, t._2 + 1), -1)
      }).map(_._1)

    val nodes = edges.flatMap(edge => Array(edge._1, edge._2)).distinct()

    // Init the node with its own id
    var label_node = nodes.zipWithUniqueId().map{
      case(node, id) => (node, (id, 0))
    }

    for (i <- 1 to iteration) {
      // group nodes with the same id
      var new_label = label_node.join(edges).map(t => (t._2._2, (t._2._1._1, i)))
      if (trace)
         new_label = new_label.zipWithTaint()
        .taint(t => {
          val taint_tuple = t._2.asInstanceOf[(_, _)]
          val node_taint = taint_tuple._2.asInstanceOf[(_, _)]._1.asInstanceOf[Array[Object]]
          val edge_taint = taint_tuple._1.asInstanceOf[Array[Object]]
          ((-1, ((node_taint ++ edge_taint).distinct, -1)), -1)
        })
        .map(_._1)
      val m_label = new_label.union(label_node)
      label_node = m_label.reduceByKey((x, y) => {
        if (x._1 != y._1) {
          if (x._1 < y._1)
            x
          else
            y
        } else {
          if (x._2 < y._2)
            x
          else
            y
        }
      }, numPartitions = partitions)
    }

    if (trace)
      label_node.map(t => (t._1, t._2._1)).zipWithTaint().collect().foreach(println)
    else
      label_node.saveAsObjectFile("graph_out")

    readLine()

    spark.stop()
  }
}
