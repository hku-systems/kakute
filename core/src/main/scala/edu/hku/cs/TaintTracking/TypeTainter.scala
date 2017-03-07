
package edu.hku.cs.TaintTracking

import edu.hku.cs.Optimization.RuleCollector

/**
  * Created by jianyu on 3/3/17.
  */

/**
  * A [[BaseTainter]] provide basic variables and functions to add
  * Taints to a tuple, should be override by its subclasses
*/

abstract class BaseTainter {
  val TAINT_START_KEY : Int = 1
  val TAINT_START_VALUE : Int = 0x4fffffff + 1

  def setTaint[T](obj: T): T
  def getTaintList(obj: Any): RuleCollector.Rule
  def getTaintAndReturn[T](obj: T): T

  def decomposeTaint(tag: Int): List[Int] = {
    var seq = List[Int]()
    for (i <- 0 until 30) {
      if ((tag & (1 << i)) != 0)
        seq = (i + 1) :: seq
    }
    seq
  }

}

class TaintException(string: String) extends Exception {}