package edu.hku.cs.dft.tracker

import edu.columbia.cs.psl.phosphor.runtime.{MultiTainter, Taint, Tainter}
import edu.hku.cs.dft.tracker.TrackingTaint.TrackingTaint

/**
  * Created by jianyu on 4/17/17.
  */

// abstract handle for lower tainter

class CombinedTaint[T](taint: T) extends Iterable[Any] with Serializable{

  private val tt: T = taint

  def getTaint: T = tt

  override def iterator: Iterator[_] = {
    tt match {
      case null =>
        List(0).toIterator
      case ta: Int =>
        var rs: List[Int] = List()
        for (j <- 0 to 31) {
          if ((ta & (1 << j)) != 0) {
            rs = (1 << j) :: rs
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

  override def equals(obj: scala.Any): Boolean = {
    tt match {
      case int: Int =>
        obj match {
          case taint: Int => taint == int
          case _ => false
        }
      case t: Taint[_] =>
        obj match {
          case taint: Taint[_] => taint.lbl == t.lbl
          case _ => false
        }
      case _ => false
    }
  }
}

/**
  * A [[TainterHandle]] is used to set/set taint to/from a data
*/

trait TainterHandle {

  val MAX_TAINT = 0

  def setTaint[T](anyRef: T, taint: Any): T

  def getTaint(any: Any): CombinedTaint[_]

}

class ObjectTainter extends TainterHandle {

  override val MAX_TAINT: Int = 1 << 31

  private def setObjTaint[T](anyRef: T, taint: Any): T = {

    val r = anyRef match {
      case int: Int => MultiTainter.taintedInt(int, taint)
      case short: Short => MultiTainter.taintedShort(short, taint)
      case long: Long => MultiTainter.taintedLong(long, taint)
      case double: Double => MultiTainter.taintedDouble(double, taint)
      case float: Float => MultiTainter.taintedFloat(float, taint)
      case char: Char => MultiTainter.taintedChar(char, taint)
      case byte: Byte => MultiTainter.taintedByte(byte, taint)
      case boolean: Boolean => MultiTainter.taintedBoolean(boolean, taint)
      case arr: Array[Int] => MultiTainter.taintedIntArray(arr, taint)
      case arr: Array[Short] => MultiTainter.taintedShortArray(arr, taint)
      case arr: Array[Long] => MultiTainter.taintedLongArray(arr, taint)
      case arr: Array[Double] => MultiTainter.taintedDoubleArray(arr, taint)
      case arr: Array[Float] => MultiTainter.taintedFloatArray(arr, taint)
      case arr: Array[Char] => MultiTainter.taintedCharArray(arr, taint)
      case arr: Array[Byte] => MultiTainter.taintedByteArray(arr, taint)
      case arr: Array[Boolean] => MultiTainter.taintedBooleanArray(arr, taint)
      case null => null
      case obj =>
        MultiTainter.taintedObject(obj, new Taint(taint))
        obj
      case _ => throw new IllegalArgumentException("type mismatch")
    }
    r.asInstanceOf[T]
  }

  private def setObjTaint[T](anyRef: T, taint: Taint[_]): T = {
    if (taint == null)
      anyRef
    else {
      val r = anyRef match {
        case int: Int => MultiTainter.taintedInt(int, taint)
        case short: Short => MultiTainter.taintedShort(short, taint)
        case long: Long => MultiTainter.taintedLong(long, taint)
        case double: Double => MultiTainter.taintedDouble(double, taint)
        case float: Float => MultiTainter.taintedFloat(float, taint)
        case char: Char => MultiTainter.taintedChar(char, taint)
        case byte: Byte => MultiTainter.taintedByte(byte, taint)
        case boolean: Boolean => MultiTainter.taintedBoolean(boolean, taint)
        case arr: Array[_] => throw new IllegalArgumentException("could not set taint to arr")
        case null => null
        case obj =>
          MultiTainter.taintedObject(obj, taint)
          obj
        case _ => throw new IllegalArgumentException("type mismatch")
      }
      r.asInstanceOf[T]
    }
  }

  override def setTaint[T](anyRef: T, taint: Any): T = {

    taint match {
      case ct: CombinedTaint[_] =>
        setObjTaint(anyRef, ct.getTaint.asInstanceOf[Taint[_]])
      case obj: Object => setObjTaint(anyRef, obj)
      case _ => throw new IllegalArgumentException("illegal tag")
    }

  }

  override def getTaint(any: Any): CombinedTaint[Taint[_]] = {
    if (any == null)
      new CombinedTaint(null)
    else {
      val t = MultiTainter.getTaint(any)
      new CombinedTaint(t)
    }
  }
}

class IntTainter extends TainterHandle {

  private def setIntTaint[T](anyRef: Any, int: Int): T = {
    val r = anyRef match {
      case in: Int => Tainter.taintedInt(in, int)
      case short: Short => Tainter.taintedShort(short, int)
      case long: Long => Tainter.taintedLong(long, int)
      case double: Double => Tainter.taintedDouble(double, int)
      case float: Float => Tainter.taintedFloat(float, int)
      case char: Char => Tainter.taintedChar(char, int)
      case byte: Byte => Tainter.taintedByte(byte, int)
      case boolean: Boolean => Tainter.taintedBoolean(boolean, int)
      case arr: Array[Int] => Tainter.taintedIntArray(arr, int)
      case arr: Array[Short] => Tainter.taintedShortArray(arr, int)
      case arr: Array[Long] => Tainter.taintedLongArray(arr, int)
      case arr: Array[Double] => Tainter.taintedDoubleArray(arr, int)
      case arr: Array[Float] => Tainter.taintedFloatArray(arr, int)
      case arr: Array[Char] => Tainter.taintedCharArray(arr, int)
      case arr: Array[Byte] => Tainter.taintedByteArray(arr, int)
      case arr: Array[Boolean] => Tainter.taintedBooleanArray(arr, int)
      case null => null
      case obj =>
        Tainter.taintedObject(obj, int)
        obj
      case _ => throw new IllegalArgumentException("type mismatch")
    }
    r.asInstanceOf[T]
  }

  override def setTaint[T](anyRef: T, taint: Any): T = {
    taint match {
      case int: Int => setIntTaint(anyRef, int)
      case ct: CombinedTaint[Int] => setIntTaint(anyRef, ct.getTaint)
      case _ => throw new IllegalArgumentException("only int taint is supported")
    }
  }

  override def getTaint(any: Any): CombinedTaint[_] = {
    if (any == null) {
      new CombinedTaint[Int](0)
    } else {
      new CombinedTaint[Int](Tainter.getTaint(any))
    }
  }
}

object TainterHandle {

  def trackingTaint: TrackingTaint =
    if (edu.columbia.cs.psl.phosphor.Configuration.MULTI_TAINTING)
      TrackingTaint.ObjTaint
    else
      TrackingTaint.IntTaint

}