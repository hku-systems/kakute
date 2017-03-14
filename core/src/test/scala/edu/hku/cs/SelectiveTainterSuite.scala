package edu.hku.cs

import edu.hku.cs.TaintTracking.SelectiveTainter

/**
  * Created by jianyu on 3/14/17.
  */

object SelectiveTainterSuite {
  def main(args: Array[String]): Unit = {
    val k = (1, 2, 3)
    val k_2 = (2, 3, 4)
    val selectiveTainter = new SelectiveTainter(Map(), 1)
    selectiveTainter.setTaint(k)
    val k_taint = selectiveTainter.setTaint(k_2)
    val taint = selectiveTainter.getTaintList(k_taint)
    val kk = selectiveTainter.setTaintWithTaint(k, taint)
    val taint_after = selectiveTainter.getTaintList(kk)
    val look = 0
  }
}