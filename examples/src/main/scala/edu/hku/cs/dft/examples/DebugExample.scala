package edu.hku.cs.dft.examples

import org.apache.spark.{SparkConf, SparkContext}

/**
  * Created by jianyu on 4/13/17.
  */
object DebugExample {

  def main(args: Array[String]): Unit = {

    val conf = new SparkConf()
      .setAppName("Debug Example")
      .set("spark.dft.tracking.mode", "debug")
    val sc = new SparkContext(conf)

    val textFile = if (args.length > 0) args(0) else throw new IllegalArgumentException("not enough argument")

    val text = sc.textFile(textFile)

    // some data is not int
    text.flatMap(t => t.split(" ")).map(t => {
      t.toInt
    }).collect()

    sc.stop()

  }

}
