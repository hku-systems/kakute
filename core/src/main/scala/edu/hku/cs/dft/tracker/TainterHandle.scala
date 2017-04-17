package edu.hku.cs.dft.tracker

import java.util

import edu.columbia.cs.psl.phosphor.runtime.{MultiTainter, Taint}
import edu.columbia.cs.psl.phosphor.struct.LinkedList

/**
  * Created by jianyu on 4/17/17.
  */

// abstract handle for lower tainter

class CombinedTaint[T](val taint: T) extends Iterable[Int] with Serializable{

  override def iterator: Iterator[Int] = {
    taint match {
      case null =>
        List(0).toIterator
      case i: Int =>
        var rs: List[Int] = List()
        for (i <- 0 to 31) {
          if ((i & (1 << i)) != 0) {
            rs = i :: rs
          }
        }
        rs.toIterator
      case taint: Taint[_] =>
        val linkedList = taint.getDependencies
        var rs: List[Int] = List()

        taint.lbl match {
          case int: Int => rs = int :: rs
          case _ =>
        }

        var head = linkedList.getFirst
        while (head != null) {
          head.entry match {
            case int: Int => rs = int :: rs
            case _ =>
          }
          head = head.next
        }
        rs.toIterator
      case _ =>
        None.toIterator
    }
  }
}

trait TainterHandle {

  val MAX_TAINT = 0

  def setTaint[T](anyRef: T, taint: Any): T

  def setArrTaint[T](arr: T, taint: Any): T

  def getTaint(any: Any): CombinedTaint[_]

}

class ObjectTainter extends TainterHandle {

  override val MAX_TAINT: Int = 1 << 31

  override def setTaint[T](anyRef: T, taint: Any): T = {

    taint match {
      case _: Int =>
      case _: CombinedTaint[Any] =>
      case _ => throw new IllegalArgumentException("illegal tag")
    }

    val r = anyRef match {
      case int: Int => MultiTainter.taintedInt(int, taint)
      case short: Short => MultiTainter.taintedShort(short, taint)
      case long: Long => MultiTainter.taintedLong(long, taint)
      case double: Double => MultiTainter.taintedDouble(double, taint)
      case float: Float => MultiTainter.taintedFloat(float, taint)
      case char: Char => MultiTainter.taintedChar(char, taint)
      case byte: Byte => MultiTainter.taintedByte(byte, taint)
      case boolean: Boolean => MultiTainter.taintedBoolean(boolean, taint)
      case null => null
      case obj =>
        MultiTainter.taintedObject(obj, new Taint(taint))
        obj
      case _ => throw new IllegalArgumentException("type mismatch")
    }
    r.asInstanceOf[T]
  }

  override def setArrTaint[T](arr: T, taint: Any): T = {

    taint match {
      case _: Int =>
      case _: CombinedTaint[Any] =>
      case _ => throw new IllegalArgumentException("illegal tag")
    }

    val r = arr match {
      case arr: Array[Int] => MultiTainter.taintedIntArray(arr, taint)
      case arr: Array[Short] => MultiTainter.taintedShortArray(arr, taint)
      case arr: Array[Long] => MultiTainter.taintedLongArray(arr, taint)
      case arr: Array[Double] => MultiTainter.taintedDoubleArray(arr, taint)
      case arr: Array[Float] => MultiTainter.taintedFloatArray(arr, taint)
      case arr: Array[Char] => MultiTainter.taintedCharArray(arr, taint)
      case arr: Array[Byte] => MultiTainter.taintedByteArray(arr, taint)
      case arr: Array[Boolean] => MultiTainter.taintedBooleanArray(arr, taint)
      case _ => throw new IllegalArgumentException("type mismatch")
    }
    r.asInstanceOf[T]
  }

  override def getTaint(any: Any): CombinedTaint[_] = {
    if (any == null)
      new CombinedTaint(null)
    else {
      val t = MultiTainter.getTaint(any)
      new CombinedTaint(t)
    }
  }
}
