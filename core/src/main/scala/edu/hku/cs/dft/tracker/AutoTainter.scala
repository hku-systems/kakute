package edu.hku.cs.dft.tracker

import java.io.{BufferedReader, File, FileReader}
import scala.util.control.Breaks._
/**
  * Created by max on 10/4/2017.
  */

/**
  * A auto tainter add tag to input automatically, according to
  * user rule
*/

trait AutoTainter {

  // read configuration file
  def initEngine(): Unit

  def setTaint[T](t: T): T

}

class FullAutoTainter extends AutoTainter {

  private val autoTaint = 1 << 2

  private val selectiveTainter: SelectiveTainter =
    new SelectiveTainter(Map(), autoTaint)

  override def initEngine(): Unit = {}

  override def setTaint[T](t: T):T  = {
    selectiveTainter.setTaint(t)
  }

}

class TextAutoTainter(confFile: String) extends AutoTainter {

  /**
    * conf file format
    * separator = regularExpression
    * columns = user,name,gender(separatedd by comma)
    * taints = 1, 2, 3, 4(seperated by comma)
  */

  var seperator: String = _

  var columns: List[String] = _

  var taints: Vector[Int] = Vector()

  val selectiveTainter: SelectiveTainter = new SelectiveTainter(Map(), 0)

  override def initEngine(): Unit = {
    val cfReadfer = new BufferedReader(new FileReader(new File(confFile)))
    breakable {
      while (true) {
        val line = cfReadfer.readLine()
        if (line == null) break()
        if (!line.startsWith("#")) {
          val keyValue = line.split("=")
          val key = keyValue(0).trim()
          val value = keyValue(1).trim()
          key match {
            case "separator" => seperator = value.asInstanceOf[String]
            case "columns" =>
              columns = value.split(",").toList
            case "taints" => taints = value.split(",")
              .map(t => t.toInt).toVector
          }
        }
      }
    }

    taints.foreach(t => {
      println("add taint " + t)
    })
  }

  override def setTaint[T](t: T): T = {
    t match {
      case s: String =>
        val splitString = s.split(seperator)
        val stringBuilder = new StringBuilder
        for(ss: (String, Int) <- splitString.zipWithIndex) {
          selectiveTainter.setFilter(Map[Int, Any => Int](0 -> ((_: Any) => ss._2)))
          stringBuilder.append(selectiveTainter.setTaint(ss + seperator))
        }
        splitString.toString.asInstanceOf[T]
      case _ => throw new IllegalArgumentException("only string is supported")
    }
  }

}
