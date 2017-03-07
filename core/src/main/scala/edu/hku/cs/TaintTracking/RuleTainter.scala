package edu.hku.cs.TaintTracking

import edu.columbia.cs.psl.phosphor.runtime.Tainter
import edu.hku.cs.Optimization.RuleCollector
import edu.hku.cs.Optimization.RuleCollector.Rule

/**
  * Created by jianyu on 3/7/17.
  */

class RuleTainter(trackingPolicy: TrackingPolicy, ruleCollector: RuleCollector) extends BaseTainter{

  val policy: TrackingPolicy = trackingPolicy

  var currentTaintKey: Int = TAINT_START_KEY

  var currentTaintValue: Int = TAINT_START_VALUE

  var currentGetIndex: Int = 0

  private var deps: RuleCollector.Rule = _

  def currentKeyIndex(): Int = {
    if (currentTaintKey >= currentTaintValue) throw new TaintException("Not enough keys")
    val returnTaint = currentTaintKey
    currentTaintKey <<= 1
    returnTaint
  }

  def currentValueIndex(): Int = {
    /*  if (_currentTaintKey >= _currentTaintValue)
          return 0
        val returnTaint = _currentTaintValue
        _currentTaintValue >>= 1
        returnTaint*/
    currentKeyIndex()
  }

  def currentIndex(): Int = {
    val returnCurrent = currentGetIndex
    currentGetIndex += 1
    returnCurrent
  }

  def taintOne[T](obj: T): T = {
    if (policy._tracking_type == TrackingType.Keys) {
      obj match {
        case int: Int => Tainter.taintedInt(int, currentKeyIndex()).asInstanceOf[T]
        case long: Long => Tainter.taintedLong(long, currentKeyIndex()).asInstanceOf[T]
        case short: Short => Tainter.taintedShort(short, currentKeyIndex()).asInstanceOf[T]
        case float: Float => Tainter.taintedFloat(float, currentKeyIndex()).asInstanceOf[T]
        case double: Double => Tainter.taintedDouble(double, currentKeyIndex()).asInstanceOf[T]
        case bool: Boolean => Tainter.taintedBoolean(bool, currentKeyIndex()).asInstanceOf[T]
        case char: Char => Tainter.taintedChar(char, currentKeyIndex()).asInstanceOf[T]
        case byte: Byte => Tainter.taintedByte(byte, currentKeyIndex()).asInstanceOf[T]
        case _ => obj
      }
    } else if (policy._tracking_type == TrackingType.Values) {
      obj match {
        case int: Array[Int] => Tainter.taintedIntArray(int, currentValueIndex()).asInstanceOf[T]
        case long: Array[Long] => Tainter.taintedLongArray(long, currentValueIndex()).asInstanceOf[T]
        case short: Array[Short] => Tainter.taintedShortArray(short, currentValueIndex()).asInstanceOf[T]
        case float: Array[Float] => Tainter.taintedFloatArray(float, currentValueIndex()).asInstanceOf[T]
        case double: Array[Double] => Tainter.taintedDoubleArray(double, currentValueIndex()).asInstanceOf[T]
        case bool: Array[Boolean] => Tainter.taintedBooleanArray(bool, currentValueIndex()).asInstanceOf[T]
        case char: Array[Char] => Tainter.taintedCharArray(char, currentValueIndex()).asInstanceOf[T]
        case byte: Array[Byte] => Tainter.taintedByteArray(byte, currentValueIndex()).asInstanceOf[T]
        /* Object */
        case obj: Object => obj.asInstanceOf[T]
        case _ => obj
      }
    } else if (policy._tracking_type == TrackingType.KeyValues) {
      obj match {
        // Primitive
        case int: Int => Tainter.taintedInt(int, currentKeyIndex()).asInstanceOf[T]
        case long: Long => Tainter.taintedLong(long, currentKeyIndex()).asInstanceOf[T]
        case short: Short => Tainter.taintedShort(short, currentKeyIndex()).asInstanceOf[T]
        case float: Float => Tainter.taintedFloat(float, currentKeyIndex()).asInstanceOf[T]
        case double: Double => Tainter.taintedDouble(double, currentKeyIndex()).asInstanceOf[T]
        case bool: Boolean => Tainter.taintedBoolean(bool, currentKeyIndex()).asInstanceOf[T]
        case char: Char => Tainter.taintedChar(char, currentKeyIndex()).asInstanceOf[T]
        case byte: Byte => Tainter.taintedByte(byte, currentKeyIndex()).asInstanceOf[T]

        // Array and Object
        case int: Array[Int] => Tainter.taintedIntArray(int, currentValueIndex()).asInstanceOf[T]
        case long: Array[Long] => Tainter.taintedLongArray(long, currentValueIndex()).asInstanceOf[T]
        case short: Array[Short] => Tainter.taintedShortArray(short, currentValueIndex()).asInstanceOf[T]
        case float: Array[Float] => Tainter.taintedFloatArray(float, currentValueIndex()).asInstanceOf[T]
        case double: Array[Double] => Tainter.taintedDoubleArray(double, currentValueIndex()).asInstanceOf[T]
        case bool: Array[Boolean] => Tainter.taintedBooleanArray(bool, currentValueIndex()).asInstanceOf[T]
        case char: Array[Char] => Tainter.taintedCharArray(char, currentValueIndex()).asInstanceOf[T]
        case byte: Array[Byte] => Tainter.taintedByteArray(byte, currentValueIndex()).asInstanceOf[T]
        case obj: Object => obj.asInstanceOf[T]
        case _ => obj
      }
    } else {
      obj
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

  override def setTaint[T](obj: T): T = {
    currentTaintKey = TAINT_START_KEY
    currentTaintValue = TAINT_START_VALUE
    taintAllHelper(obj)
  }

/**
  * This method report the rule generated to [[RuleCollector]], and return the Object itself
  */
  override def getTaintAndReturn[T](obj: T): T = {
    deps = List()
    currentGetIndex = 1
    val returnVal = getTaintHelper(obj)
    ruleCollector.addRule(deps)
    returnVal
  }

  /* Clear all taint in the object, using taintAll may be fine????? */
  private def getTaintHelper[T](obj: T): T = {
    obj match {
      case product: Product => product.productIterator.foreach(getTaintHelper)
      case v: Int => deps = (currentIndex(), decomposeTaint(Tainter.getTaint(v))) :: deps
      case v: Short => deps = (currentIndex(), decomposeTaint(Tainter.getTaint(v))) :: deps
      case v: Long => deps = (currentIndex(), decomposeTaint(Tainter.getTaint(v))) :: deps
      case v: Double => deps = (currentIndex(), decomposeTaint(Tainter.getTaint(v))) :: deps
      case v: Float => deps = (currentIndex(), decomposeTaint(Tainter.getTaint(v))) :: deps
      case v: Byte => deps = (currentIndex(), decomposeTaint(Tainter.getTaint(v))) :: deps
      case v: Char => deps = (currentIndex(), decomposeTaint(Tainter.getTaint(v))) :: deps
      case v: Boolean => deps = (currentIndex(), decomposeTaint(Tainter.getTaint(v))) :: deps
      case v: Object => deps = (currentIndex(), decomposeTaint(Tainter.getTaint(v))) :: deps
    }
    obj
  }

  override def getTaintList(obj: Any): Rule = {
    throw new TaintException("Not implemented")
  }
}