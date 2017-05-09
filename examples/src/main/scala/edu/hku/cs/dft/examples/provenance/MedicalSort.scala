package edu.hku.cs.dft.examples.provenance

import org.apache.spark.sql.SparkSession

/**
  * Created by max on 9/5/2017.
  */
object MedicalSort {

  def main(args: Array[String]): Unit = {

    val trace = if (args.length > 0 && args(0).equals("true")) {
      true
    } else
      false

    val session = SparkSession
      .builder()
      .appName("medical sort")
    if (trace)
      session.config("spark.dft.tracking.mode", "full")

    val spark = session.getOrCreate()

    var records = spark.sparkContext.parallelize(Seq(
      ("A", 20, "cancer"),
      ("B", 40, "brain cancer"),
      ("C", 25, "heart disease"),
      ("D", 50, "klsdf")), 4)

    if (trace)
      records = records.zipWithUniqueId().taint(m => {
        ((m._2, -1, -1), -1)
      }).map(_._1)

    val sortedRecord = records.map(t => {
      val sort =
        if (t._2 > 15)
          1
        else if (t._2 > 40)
          2
        else if (t._2 > 60)
          3
        else
          0
      (sort, t)
    })

    val result = sortedRecord.groupByKey().flatMapValues(t => {
      t.toList.sortBy(_._1)
    })

    if (trace)
      result.collectWithTaint().foreach(println)
    else
      result.collect().foreach(println)

    readLine()

    spark.stop()
  }
}
