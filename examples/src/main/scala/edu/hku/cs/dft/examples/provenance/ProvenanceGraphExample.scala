package edu.hku.cs.dft.examples.provenance

import edu.hku.cs.dft.tracker.CombinedTaint
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
    var label_node = nodes.map(t => (t, (t, 0)))

    for (i <- 1 to 10) {
      // group nodes with the same id
      var new_label = label_node.join(edges).map(t => (t._2._2, (t._2._1._1, i)))
      if (trace)
         new_label = new_label.zipWithTaint()
        .taint(t => {
          var taint_set: Set[Long] = Set()
          val taint_tuple = t._2.asInstanceOf[(_, _)]
          var node_taint = taint_tuple._2.asInstanceOf[CombinedTaint[_]].iterator.toSet
          val edge_taint = taint_tuple._1.asInstanceOf[CombinedTaint[_]].iterator.toList
          node_taint.foreach(t => t match {
            case s: Set[Long] => taint_set = taint_set ++ s
            case k: Long => taint_set = taint_set + k
            case _ => throw new IllegalArgumentException("illegal")
          })
          taint_set = taint_set + edge_taint.head.asInstanceOf[Long]
          ((-1, taint_set), -1)
        })
        .map(_._1)
      val m_label = new_label.union(label_node)
      label_node = m_label.reduceByKey(math.min)
    }

    if (trace)
      label_node.collectWithTaint().foreach(t => {
        print(t._1 + " ")
        val taint_tuple = t._2.asInstanceOf[(_, _)]
        print(taint_tuple._1 + " ")
        val maybeList = taint_tuple._2.asInstanceOf[CombinedTaint[_]].iterator.toList
        if (maybeList.nonEmpty)
          maybeList.head.asInstanceOf[Set[_]].foreach(m => print(m + " "))
        println()
      })
    else
      label_node.collect().foreach(println)

    readLine()

    spark.stop()
  }
}
