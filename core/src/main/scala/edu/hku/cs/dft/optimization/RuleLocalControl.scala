package edu.hku.cs.dft.optimization

import edu.hku.cs.dft.network._
import edu.hku.cs.dft.network.NettyEndpoint

/**
  * Created by jianyu on 3/9/17.
  */

class SplitCollector(_split: Int) {
  var ruleCollector: Map[Int, RuleCollector] = Map()
  val split:Int = _split
  def collectorInstance(_id: Int): RuleCollector = {
    val r = ruleCollector.getOrElse(_id, new RuleCollector(_id))
    ruleCollector += _id -> r
    r
  }
}

//TODO Is this class atomic?
class RuleLocalControl extends NettyEndpoint{

  override val id: String = "Rule"

  override def receiveAndReply(message: Message): Message = {
    message match {
      case registered: RuleRegistered => null
      //TODO put the non-confirm rule to a pool and resent - need to do this?
      case added: RuleAdded => null
    }
  }

  override def onRegister(): Unit = {
    this.send(RuleRegister(true))
  }

  private var collector = 0

  private var splitCollectors: Map[Int, SplitCollector] = Map()

  private var typeCollectors: Map[Int, String] = Map()

  def addType(id: Int, string: String): Unit = {
    typeCollectors += id -> string
  }

  def splitInstance(_id: Int): SplitCollector = {
    synchronized {
      val r = splitCollectors.getOrElse(_id, new SplitCollector(_id))
      splitCollectors += _id -> r
      r
    }
  }

  def collect(_split: Int): Unit = {
    val splitCollector = synchronized {
      val got = splitCollectors.getOrElse(_split, null)
      splitCollectors -= _split
      got
    }
    if (splitCollector != null) {
      splitCollector.ruleCollector.foreach(mm => {
        println("new " + _split + " " + mm._1 + " " + mm._2.collect())
        this.send(RuleInfered(mm._1, mm._2.collect()))
      })
    }
    typeCollectors.foreach(t => {
      this.send(DataType(t._1, t._2))
    })
  }

}
