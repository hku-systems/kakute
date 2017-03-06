package edu.hku.cs.Optimization

/**
  * Created by jianyu on 3/4/17.
  */

// each manager is for one stage
class RuleManager(id: Int) {
  private var collector = 0
  private var ruleCollectors: Map[Int, RuleCollector] = Map()
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
      println("Infer " + mm._1 + " " + mm._2.collect())
    })
    ruleCollectors = Map()
  }
}

object RuleManager {
  private var manager = 0
  private var ruleManagers: Map[Int, RuleManager] = Map()
  def managerInstance(_id: Int): RuleManager = {
    val foundManager = ruleManagers.find(_._1 == _id)
    val ruleManager = if (foundManager.isEmpty) {
      manager += 1
      _id -> new RuleManager(_id)
    } else {
      foundManager.get
    }
    ruleManagers += ruleManager
    ruleManager._2
  }
}
