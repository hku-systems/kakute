package edu.hku.cs.dft.examples

import edu.hku.cs.dft.tracker.CombinedTaint
import org.apache.spark.{SparkConf, SparkContext}

/**
  * Created by jianyu on 5/3/17.
  */
object TwitterLineage {
  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
    val twitterfile = args(0)

    val sc = new SparkContext(conf)

    var file = sc.textFile(twitterfile, 10)

    val taint_file = file.zipWithUniqueId().taint(t => {
      (t._2, -1)
    })
      .map(_._1)

    val to = taint_file.map(
      line => {
        val list = line.split(",")
        if(list.size > 4){
          val time = list(0).split(" ")(3).split(":")(1)
          val text = list(3)
          (time, text)
        }
        else{
          ("15:02:45","  ")
        }
      }

    )

    val file1 = to.groupByKey()


    val fm = file1.flatMap(line =>
      line._2.flatMap(l => l.trim().split(" ")))
    val pair = fm.map{word =>
      (word, if (word.length > 0)(word.charAt(0).toInt | 1) & 1 else 1)
    }

    val count = pair.reduceByKey(_ + _)
        .zipWithTaint().map(t => {
      val taintTuple = t._2.asInstanceOf[(CombinedTaint[_], CombinedTaint[_])]
      (t._1, taintTuple._2.iterator.toArray)
    })
    count.collect().foreach(t => {
      println(t._1 + " " + t._2.length)
    }
    )

    sc.stop()
  }
}
