package edu.hku.cs.dft.examples

import edu.hku.cs.dft.tracker.CombinedTaint
import org.apache.spark.{SparkConf, SparkContext}

/**
  * Created by jianyu on 5/1/17.
  */
object WordCountLineageExample {
  def main(args: Array[String]): Unit = {

    val intput = args(0)

    val conf = new SparkConf
    val sc = new SparkContext(conf)
    val text = sc.textFile(intput)
    val text_taint = text.zipWithUniqueId().taint(t => {
      (t._2, 0)
    })

    text_taint.map(t => (0, t._1))
        .groupByKey()
        .flatMap(it =>
          it._2.flatMap(t => t.split("\\s+"))
        )
      .map(t => (t, (t.charAt(0).toInt | 1) & 1))
      .reduceByKey(_ + _)
        .filter(t => t._1.contains("Racism"))
      .collectWithTaint().foreach(println)
    sc.stop()
  }
}

