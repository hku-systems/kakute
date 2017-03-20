package edu.hku.cs.dft

import java.io.FileNotFoundException

import edu.hku.cs.dft.SampleMode.SampleMode
import edu.hku.cs.dft.TrackingMode.TrackingMode
import org.apache.spark.SparkConf

import scala.io.Source


/**
  * Created by max on 8/3/2017.
  */
/**
  * A [[ArgumentHandle]] specify the interface needed to parse the arguments for
  * our data-flow tracking framework
  * */

trait ArgumentHandle {
  def init(): Boolean
  def parseArgs(key: String): String
}

/**
  * Parse configuration from spark
*/


class SparkArgumentHandle(sparkConf: SparkConf) extends ArgumentHandle {
  override def init(): Boolean = true

  override def parseArgs(key: String): String = {
    sparkConf.get("spark.dft." + key)
  }
}

class ConfFileHandle(filename: String) extends ArgumentHandle with DFTEnv.DFTLoggig {

  var keyMap: Map[String, String] = Map()

  try {
    logInfo("Read configuration file from " + filename)
    for (line <- Source.fromFile(filename).getLines()) {
      if (!line.trim.startsWith("#")) {
        val arr = line.split("=")
        if (arr.length >= 3) throw new Exception("wrong format")
        val key = arr(0).trim
        val value = arr(1).trim
        keyMap += key -> value
        logInfo("conf: " + key + " -> " + value)
      }
    }
  } catch {
    case e: FileNotFoundException => logInfo("conf file " + filename + " not found")
    // use the default setting
  }

  override def init(): Boolean = true

  override def parseArgs(key: String): String = {
    keyMap.getOrElse(key, null)
  }
}

/**
* Parse the configuration from code
* */
class CustomArgumentHandle extends ArgumentHandle {
  override def init(): Boolean = true

  override def parseArgs(key: String): String = {
    key match {
      case "host" => DefaultArgument.host
      case "port" => DefaultArgument.port.toString
      case "tracking" => "mix"
      case "sample" => "off"
      case "mode" => "server"
      case "phosphor_java" => ""
      case "phosphor_jar" => ""
      case "phosphor_cache" => "./phosphor_cache/"
    }
  }
}

object DefaultArgument {
  val host: String = "127.0.0.1"
  val port: Int = 8787
  val trackingMode: TrackingMode = TrackingMode.RuleTracking
  val sampleMode: SampleMode = SampleMode.Off
  val mode: String = "server"
  // By default, 10% of the data is used when in the data sampling mode
  val sampleInt: Int = 10
}