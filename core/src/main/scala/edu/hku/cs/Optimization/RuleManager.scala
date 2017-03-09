package edu.hku.cs.Optimization

import edu.hku.cs.network.RuleInfered

/**
  * Created by jianyu on 3/4/17.
  */

/**
  * A [[RuleManager]] manages a stage(tasks)
 */
class RuleManager(id: Int, local: RuleLocalControl) {
  private var collector = 0

  private var ruleCollectors: Map[Int, RuleCollector] = Map()

  private val localControl: RuleLocalControl = local

  def collectorInstance(_id: Int): RuleCollector = {
    val foundCollector = ruleCollectors.find(_._1 == id)
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
      localControl.send(RuleInfered(mm._1, mm._2.collect()))
    })
    ruleCollectors = Map()
  }
}

