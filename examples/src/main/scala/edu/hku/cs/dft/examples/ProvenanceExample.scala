package edu.hku.cs.dft.examples

import org.apache.spark.sql.SparkSession

/**
  * Created by max on 21/4/2017.
  */
object ProvenanceExample {
  case class Tracer(objId: Int, key: Int)
  def main(args: Array[String]): Unit = {
    val spark = SparkSession
      .builder()
      .appName("Provenance Example")
      .config("spark.dft.tracking.mode", "full")
      .getOrCreate()

    val file = if (args.length > 0)
      args(0)
    else
      throw new Exception("please give input filename")

    val text = spark.read.textFile(file).rdd
    val lines = text.flatMap(t => t.split("\\s+"))
    val words = lines.map(word => (word, 1))
    val wordCount = words.reduceByKey(_ + _)
    var counter = 0
    val taintedCount = wordCount.taint(t => {
      counter += 1
      (Tracer(counter, 1), Tracer(counter, 1))
    })

    val result = taintedCount.reduceByKey(_ + _)
    val result_taint = result.collectWithTaint()
    result_taint.foreach(r => {
      println(r._1 + ":")
      r._2.foreach(t => {
        print(t._1 + ": ")
        t._2.foreach(m => print(" " + m))
        println()
      })
    })

    spark.stop()
  }
}