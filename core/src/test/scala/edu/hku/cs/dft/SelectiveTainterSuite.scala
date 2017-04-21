package edu.hku.cs.dft

import edu.hku.cs.dft.tracker.SelectiveTainter


/**
  * Created by jianyu on 3/14/17.
  */

object SelectiveTainterSuite {

  case class TA(int: Int, k: Int)
  def main(args: Array[String]): Unit = {
    val k = (1, 2, 3)
    val k_2 = (2, 3, 4)
    val selectiveTainter = new SelectiveTainter(Map(), 1)

    val aa = (1, 2)
    val tai = (new TA(1, 2), new TA(2, 3))

    val aat = selectiveTainter.setTaintWithTupleTaint(aa, tai)
    val aaat = selectiveTainter.getTaintList(aat)
    val tupleAAAt = selectiveTainter.getTupleTaint(aat)
    selectiveTainter.setTaint(k)
    val k_taint = selectiveTainter.setTaint(k_2)
    val taint = selectiveTainter.getTaintList(k_taint)
    val kk = selectiveTainter.setTaintWithTaint(k, taint)
    val taint_after = selectiveTainter.getTaintList(kk)
    val a = (1, 1)
    val b = (TA(1, 2), 1)
    val tupleTainted = selectiveTainter.setTaintWithTupleTaint(a, b)
    val tupleTaints = selectiveTainter.getTaintList(tupleTainted)
//    assert(selectiveTainter.getTaintList(tupleTainted) == Map(1 -> 1, 2-> 2, 3 -> 2))

    val g = List(1, 2, 3, 4).toIterator
    val g_t = selectiveTainter.setTaintWithTupleTaint(g, 2)
    val taintR = g_t.foreach(t => println(selectiveTainter.getTaintList(t)))
    val look = 0
  }
}