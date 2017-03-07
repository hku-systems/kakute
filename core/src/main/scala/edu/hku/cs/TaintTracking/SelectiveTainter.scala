package edu.hku.cs.TaintTracking
import edu.columbia.cs.psl.phosphor.runtime.Tainter
import edu.hku.cs.Optimization.RuleCollector.Rule

/**
  * Created by jianyu on 3/7/17.
  */

/**
* A [[SelectiveTainter]] add to taint to a specific record,
* when a record fullfill some conditions
*/

class SelectiveTainter extends BaseTainter{

  override def setTaint[T](obj: T): T = {
    throw new TaintException("Not implemented")
  }

  private var _index = 0

  private var _tagPosition: Set[(Int, Int)] = _

  def taintOne[T](obj: T): T = {
    _index += 1
    var tag = 0
    if (_tagPosition.exists(k => {
      tag = k._2
      k._1 == _index
    })) {
      obj match {
        // Primitive
        case int: Int => Tainter.taintedInt(int, tag).asInstanceOf[T]
        case long: Long => Tainter.taintedLong(long, tag).asInstanceOf[T]
        case short: Short => Tainter.taintedShort(short, tag).asInstanceOf[T]
        case float: Float => Tainter.taintedFloat(float, tag).asInstanceOf[T]
        case double: Double => Tainter.taintedDouble(double, tag).asInstanceOf[T]
        case bool: Boolean => Tainter.taintedBoolean(bool, tag).asInstanceOf[T]
        case char: Char => Tainter.taintedChar(char, tag).asInstanceOf[T]
        case byte: Byte => Tainter.taintedByte(byte, tag).asInstanceOf[T]

        // Array and Object
        case int: Array[Int] => Tainter.taintedIntArray(int, tag).asInstanceOf[T]
        case long: Array[Long] => Tainter.taintedLongArray(long, tag).asInstanceOf[T]
        case short: Array[Short] => Tainter.taintedShortArray(short, tag).asInstanceOf[T]
        case float: Array[Float] => Tainter.taintedFloatArray(float, tag).asInstanceOf[T]
        case double: Array[Double] => Tainter.taintedDoubleArray(double, tag).asInstanceOf[T]
        case bool: Array[Boolean] => Tainter.taintedBooleanArray(bool, tag).asInstanceOf[T]
        case char: Array[Char] => Tainter.taintedCharArray(char, tag).asInstanceOf[T]
        case byte: Array[Byte] => Tainter.taintedByteArray(byte, tag).asInstanceOf[T]
        case obj: Object => obj.asInstanceOf[T]
        case _ => obj
      }
    }
  }

  private def taintAllHelper[T](obj: T): T = {
    obj match {
      /* Product, Scala Allow only 22 elements in a tuple */
      case (_1, _2) => (taintAllHelper(_1), taintAllHelper(_2)).asInstanceOf[T]
      case (_1, _2, _3) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3)).asInstanceOf[T]
      case (_1, _2, _3, _4) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3), taintAllHelper(_4)).asInstanceOf[T]
      case (_1, _2, _3, _4, _5) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3), taintAllHelper(_4), taintAllHelper(_5)).asInstanceOf[T]
      case (_1, _2, _3, _4, _5, _6) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3), taintAllHelper(_4), taintAllHelper(_5), taintAllHelper(_6)).asInstanceOf[T]
      case (_1, _2, _3, _4, _5, _6, _7) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3), taintAllHelper(_4), taintAllHelper(_5), taintAllHelper(_6), taintAllHelper(_7)).asInstanceOf[T]
      case (_1, _2, _3, _4, _5, _6, _7, _8) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3), taintAllHelper(_4), taintAllHelper(_5), taintAllHelper(_6), taintAllHelper(_7), taintAllHelper(_8)).asInstanceOf[T]
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3), taintAllHelper(_4), taintAllHelper(_5), taintAllHelper(_6), taintAllHelper(_7), taintAllHelper(_8), taintAllHelper(_9)).asInstanceOf[T]
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3), taintAllHelper(_4), taintAllHelper(_5), taintAllHelper(_6), taintAllHelper(_7), taintAllHelper(_8), taintAllHelper(_9), taintAllHelper(_10)).asInstanceOf[T]
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3), taintAllHelper(_4), taintAllHelper(_5), taintAllHelper(_6), taintAllHelper(_7), taintAllHelper(_8), taintAllHelper(_9), taintAllHelper(_10), taintAllHelper(_11)).asInstanceOf[T]
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3), taintAllHelper(_4), taintAllHelper(_5), taintAllHelper(_6), taintAllHelper(_7), taintAllHelper(_8), taintAllHelper(_9), taintAllHelper(_10), taintAllHelper(_11), taintAllHelper(_12)).asInstanceOf[T]
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3), taintAllHelper(_4), taintAllHelper(_5), taintAllHelper(_6), taintAllHelper(_7), taintAllHelper(_8), taintAllHelper(_9), taintAllHelper(_10), taintAllHelper(_11), taintAllHelper(_12), taintAllHelper(_13)).asInstanceOf[T]
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3), taintAllHelper(_4), taintAllHelper(_5), taintAllHelper(_6), taintAllHelper(_7), taintAllHelper(_8), taintAllHelper(_9), taintAllHelper(_10), taintAllHelper(_11), taintAllHelper(_12), taintAllHelper(_13), taintAllHelper(_14)).asInstanceOf[T]
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3), taintAllHelper(_4), taintAllHelper(_5), taintAllHelper(_6), taintAllHelper(_7), taintAllHelper(_8), taintAllHelper(_9), taintAllHelper(_10), taintAllHelper(_11), taintAllHelper(_12), taintAllHelper(_13), taintAllHelper(_14), taintAllHelper(_15)).asInstanceOf[T]
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3), taintAllHelper(_4), taintAllHelper(_5), taintAllHelper(_6), taintAllHelper(_7), taintAllHelper(_8), taintAllHelper(_9), taintAllHelper(_10), taintAllHelper(_11), taintAllHelper(_12), taintAllHelper(_13), taintAllHelper(_14), taintAllHelper(_15), taintAllHelper(_16)).asInstanceOf[T]
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3), taintAllHelper(_4), taintAllHelper(_5), taintAllHelper(_6), taintAllHelper(_7), taintAllHelper(_8), taintAllHelper(_9), taintAllHelper(_10), taintAllHelper(_11), taintAllHelper(_12), taintAllHelper(_13), taintAllHelper(_14), taintAllHelper(_15), taintAllHelper(_16), taintAllHelper(_17)).asInstanceOf[T]
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3), taintAllHelper(_4), taintAllHelper(_5), taintAllHelper(_6), taintAllHelper(_7), taintAllHelper(_8), taintAllHelper(_9), taintAllHelper(_10), taintAllHelper(_11), taintAllHelper(_12), taintAllHelper(_13), taintAllHelper(_14), taintAllHelper(_15), taintAllHelper(_16), taintAllHelper(_17), taintAllHelper(_18)).asInstanceOf[T]
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3), taintAllHelper(_4), taintAllHelper(_5), taintAllHelper(_6), taintAllHelper(_7), taintAllHelper(_8), taintAllHelper(_9), taintAllHelper(_10), taintAllHelper(_11), taintAllHelper(_12), taintAllHelper(_13), taintAllHelper(_14), taintAllHelper(_15), taintAllHelper(_16), taintAllHelper(_17), taintAllHelper(_18), taintAllHelper(_19)).asInstanceOf[T]
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3), taintAllHelper(_4), taintAllHelper(_5), taintAllHelper(_6), taintAllHelper(_7), taintAllHelper(_8), taintAllHelper(_9), taintAllHelper(_10), taintAllHelper(_11), taintAllHelper(_12), taintAllHelper(_13), taintAllHelper(_14), taintAllHelper(_15), taintAllHelper(_16), taintAllHelper(_17), taintAllHelper(_18), taintAllHelper(_19), taintAllHelper(_20)).asInstanceOf[T]
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3), taintAllHelper(_4), taintAllHelper(_5), taintAllHelper(_6), taintAllHelper(_7), taintAllHelper(_8), taintAllHelper(_9), taintAllHelper(_10), taintAllHelper(_11), taintAllHelper(_12), taintAllHelper(_13), taintAllHelper(_14), taintAllHelper(_15), taintAllHelper(_16), taintAllHelper(_17), taintAllHelper(_18), taintAllHelper(_19), taintAllHelper(_20), taintAllHelper(_21)).asInstanceOf[T]
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22) => (taintAllHelper(_1), taintAllHelper(_2), taintAllHelper(_3), taintAllHelper(_4), taintAllHelper(_5), taintAllHelper(_6), taintAllHelper(_7), taintAllHelper(_8), taintAllHelper(_9), taintAllHelper(_10), taintAllHelper(_11), taintAllHelper(_12), taintAllHelper(_13), taintAllHelper(_14), taintAllHelper(_15), taintAllHelper(_16), taintAllHelper(_17), taintAllHelper(_18), taintAllHelper(_19), taintAllHelper(_20), taintAllHelper(_21), taintAllHelper(_22)).asInstanceOf[T]
      case _ => taintOne(obj)
    }
  }

  def setTaint[T](obj: T, conditions: List[T => Boolean], tagPosition: Set[(Int, Int)]): T = {
    _tagPosition = tagPosition
    conditions.foreach(f => {
      if (f(obj)) {
        return taintAllHelper(obj)
      }
    })
    obj
  }

  override def getTaintList(obj: Any): Rule = {
    //TODO
    null
  }

  override def getTaintAndReturn[T](obj: T): (T, Rule) = {
    throw new TaintException("Not implemented")
  }
}