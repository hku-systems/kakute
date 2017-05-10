package edu.hku.cs.dft.examples.provenance

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

    val trace = args.length > 1 && args(1).equals("true")

    if (trace)
      file = file.zipWithUniqueId().taint(t => {
      (t._2, -1)
    }).map(_._1)

    val to = file.map(
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


    val top_words = file1.map(t => {
      val words = t._2.flatMap(_.trim.split(" "))
      val sorted_arr = words.map(t => (t, if (t.length > 0) (t.charAt(0).toInt | 1) & 1 else 1))
        .groupBy(_._1).map(l => (l._1, l._2.map(_._2).sum))
        .toArray
        .sortBy(_._2)
      (t._1, sorted_arr)
    })

    val time_group = top_words.flatMapValues(t => t)

    if (trace)
      time_group.collectWithTaint().foreach(println)
    else
      time_group.collect().foreach(println)

    readLine()

    sc.stop()
  }
}
