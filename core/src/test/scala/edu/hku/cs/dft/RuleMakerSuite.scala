package edu.hku.cs.dft

import edu.hku.cs.dft.optimization.RuleMaker

/**
  * Created by jianyu on 3/21/17.
  */
object RuleMakerSuite {
  def main(args: Array[String]): Unit = {
    val typeInfo = (Int, Int, ((Double, Double), Int))
    val rule = RuleMaker.makeOneToOneRuleFromTypeInfo(typeInfo)
    assert(rule == Map(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5))
  }
}
