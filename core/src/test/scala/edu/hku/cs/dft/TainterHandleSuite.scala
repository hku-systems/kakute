package edu.hku.cs.dft

import edu.hku.cs.dft.tracker.{ObjectTainter, TainterHandle}

/**
  * Created by jianyu on 4/17/17.
  */
object TainterHandleSuite {
  def main(args: Array[String]): Unit = {
    val tainterHandle: TainterHandle = new ObjectTainter
    val taintedOne = tainterHandle.setTaint(1, 1)
    val taintedTwo = tainterHandle.setTaint(2, 2)
    val taintedThree = taintedOne + taintedTwo
    val originalTaint = tainterHandle.getTaint(taintedOne)
    val propogateTaint = tainterHandle.getTaint(taintedThree)
    val a = 1
  }
}
