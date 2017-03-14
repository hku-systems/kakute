package edu.hku.cs.Optimization

import edu.hku.cs.network._

/**
  * Created by jianyu on 3/9/17.
  */

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

  private var ruleCollectors: Map[Int, RuleCollector] = Map()

  private var typeCollectors: Map[Int, String] = Map()

  def addType(id: Int, string: String): Unit = {
    typeCollectors += id -> string
  }

  def collectorInstance(_id: Int): RuleCollector = {
    val foundCollector = ruleCollectors.find(_._1 == _id)
    val ruleCollector = if (foundCollector.isEmpty) {
      collector += 1
      _id -> new RuleCollector(_id)
    } else {
      foundCollector.get
    }
    ruleCollectors += ruleCollector
    ruleCollector._2
  }

  def collect(): Unit = {
    ruleCollectors.foreach(mm => {
      if (!mm._2.isEmpty())
        this.send(RuleInfered(mm._1, mm._2.collect()))
    })
    typeCollectors.foreach(t => {
      this.send(DataType(t._1, t._2))
    })
    ruleCollectors = Map()
  }

}
