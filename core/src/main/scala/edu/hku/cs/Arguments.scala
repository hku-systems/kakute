package edu.hku.cs

import edu.hku.cs.SampleMode.SampleMode
import edu.hku.cs.TrackingMode.TrackingMode
import org.apache.spark.SparkConf
import play.api.libs.json.Format
import play.api.libs.json._
import play.api.libs.functional.syntax._


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

case class JsonConf(server: String, port: Int, tracking: String, sample: String)

class JsonArgumentHandle(file: String) extends ArgumentHandle {
  implicit val residentReads = Json.reads[JsonConf]
  override def init(): Unit = {

  }

  override def parseArgs(key: String):String = {

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
    }
  }
}

object DefaultArgument {
  val host: String = "127.0.0.1"
  val port: Int = 8787
  val trackingMode: TrackingMode = TrackingMode.RuleTracking
  val sampleMode: SampleMode = SampleMode.Off
  val mode: String = "server"
}