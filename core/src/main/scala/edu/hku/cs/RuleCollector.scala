package edu.hku.cs

/**
  * Created by jianyu on 3/4/17.
  */

class RuleCollector(id: Int) {
  type Dependency = (Int, List[Int])
  type Rule = List[Dependency]

  private var ruleSet: List[(Rule, Int)] = List()

  def addRule(r: Rule): Unit = {
    for (i <- ruleSet.indices) {
      if (r.equals(ruleSet(i)._1)){
        val count = ruleSet(i)._2
        ruleSet = ruleSet.updated(i, (r, count + 1))
      }
      return
    }
    ruleSet = ruleSet :+ (r, 1)
  }

  def collect(): List[(Rule, Int)] = ruleSet
}
