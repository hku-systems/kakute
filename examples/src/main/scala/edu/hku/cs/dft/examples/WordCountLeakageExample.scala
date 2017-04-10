package edu.hku.cs.dft.examples

/**
  * Created by jianyu on 4/9/17.
  */

import org.apache.spark._
import java.io._


object WordCountLeakageExample {
  var leak: Boolean = false
  def main(args: Array[String]) {
    if (args.length < 1) throw new IllegalArgumentException("no enough argument")
    val input = args(0)
    leak = if (args.length > 1) args(1).toBoolean else false
    val conf = new SparkConf().setAppName("wordCountApp")
    val sc = new SparkContext(conf)
    val text =  sc.textFile(input)
    val words = text.flatMap(line => line.split(" "))
    println(words.collectWithTaint().mkString(", "))
    val wc = words.map(mm).reduceByKey{case (x, y) => x + y}
    //save to .txt file locally
    /*wc.saveAsTextFile(output)*/
    println(wc.collectWithTaint.mkString(", "))
    sc.stop()
  }
  def mm(in: String): (String, Int) ={
    //client to send individual words
    if (leak) {
      try {
        val fileOutputStream = new FileWriter(new File("leak.txt"), true)
        fileOutputStream.write(in + "\n")
        fileOutputStream.close()
      }
      catch {
        case e: Exception => println(e.getStackTrace)
      }
    }
    (in, 1)
  }
}