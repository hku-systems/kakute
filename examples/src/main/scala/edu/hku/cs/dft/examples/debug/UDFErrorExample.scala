package edu.hku.cs.dft.examples.debug

import java.io.{BufferedReader, File, FileInputStream, FileReader}
import java.text.SimpleDateFormat

import org.apache.spark.{SparkConf, SparkContext}

/**
  * Created by jianyu on 4/13/17.
  */
object UDFErrorExample {

  def main(args: Array[String]): Unit = {

    val conf = new SparkConf()
      .setAppName("flow error example")
      .set("spark.dft.tracking.mode", "debug")
    val sc = new SparkContext(conf)

    val textFile = if (args.length > 0) args(0) else throw new IllegalArgumentException("not enough argument")

    val text = sc.textFile(textFile)

    // text file store filename of the passage
    // a passage end with \n will cause the passage to be replaced by ""

    val passage = text.map(t => {
      val full_string = new StringBuilder
      val readBuffer = new BufferedReader(new FileReader(t))
      var content = readBuffer.readLine()
      while (content != null) {
        full_string.append(content)
        full_string.append("\n")
        content = readBuffer.readLine()
      }
      readBuffer.close()
      full_string.toString()
    })

    passage.map(t => {
      var input_passage = t
      var word_map = Map[String, Int]()
      if (t.endsWith("\n\n")) {
        input_passage = ""
      }
      val string_array = input_passage.split(" ")

      string_array.foreach(t => {
        if (word_map.contains(t)) {
          word_map += (t -> (word_map(t) + 1))
        } else {
          word_map += (t -> 1)
        }
      })
      word_map
    })

      .collect()
    sc.stop()

  }

}
