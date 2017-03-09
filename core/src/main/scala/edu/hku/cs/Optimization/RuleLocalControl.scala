package edu.hku.cs.Optimization

import edu.hku.cs.network.{Message, NettyEndpoint, RuleRegister, RuleRegistered}

/**
  * Created by jianyu on 3/9/17.
  */


class RuleLocalControl extends NettyEndpoint{

  override val id: String = "Rule"

  private var manager = 0

  private var ruleManagers: Map[Int, RuleManager] = Map()

  override def receiveAndReply(message: Message): Message = {
    message match {
      case registered: RuleRegistered => null
    }
  }

  override def onRegister(): Unit = {
    this.send(RuleRegister(true))
  }

  def managerInstance(_id: Int): RuleManager = {
    val foundManager = ruleManagers.find(_._1 == _id)
    val ruleManager = if (foundManager.isEmpty) {
      manager += 1
      _id -> new RuleManager(_id, this)
    } else {
      foundManager.get
    }
    ruleManagers += ruleManager
    ruleManager._2
  }

}
