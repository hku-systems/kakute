
package edu.hku.cs.dft.examples

import org.apache.spark.sql.SparkSession

/**
  * Use a simple word count to test our api
  * Usage: DebuggingTest file
  */

object DebuggingExample {
  def main(args: Array[String]): Unit = {
    val spark = SparkSession
      .builder()
      .appName("debug example")
      .getOrCreate()

    val file = if (args.length > 0)
      args(0)
    else
      throw new Exception("please give the input file")

    val text = spark.read.textFile(file).rdd
    val lines = text.flatMap(t => t.split("\\s+"))
    val words = lines.map(word => (word, 1))
    val wordCount = words.reduceByKey(_ + _)
    val result = wordCount.collectWithTaint()
    result.foreach(r => println(r._1 + " " + r._2))

    val words_taint = words.taint(t => {
      t._1 match {
        case "1" => (1, 2)
        case "2" => (4, 4)
        case _ => (0, 0)
      }
    })
    val taintedResult = words_taint.collectWithTaint()
    taintedResult.foreach(t => {
      println(t._1 + " " + t._2)
    })
    val wordCountTaint = words_taint.reduceByKey(_ + _)
    val result_taint = wordCountTaint.collectWithTaint()
    result_taint.foreach(r => {
      println(r._1 + " " + r._2)
    })

    spark.stop()
  }
}
