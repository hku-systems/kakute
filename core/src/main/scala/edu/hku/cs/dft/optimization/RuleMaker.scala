package edu.hku.cs.dft.optimization

/**
  * Created by jianyu on 3/21/17.
  */

/**
  * A [[RuleMaker]] make all kinds of
  */

object RuleMaker {

  private var indexH = 0

  private def typeInfoLengthHelper(t: Any): Unit = {
    t match {
      case p:Product => p.productIterator.foreach(typeInfoLengthHelper)
      case _ => indexH += 1
    }
  }

  private def typeInfoLength(t: Any): Int = {
    indexH = 0
    typeInfoLengthHelper(t)
    indexH
  }

  private def produceOneToOneMapping(int: Int): Map[Int, Int] = {
    var r: Map[Int, Int] = Map()
    for(i <- 1 to int) {
      r += i -> i
    }
    r
  }

  def makeOneToOneRuleFromTypeInfo(t: Any): Map[Int, Int] = {
    produceOneToOneMapping(typeInfoLength(t))
  }

}