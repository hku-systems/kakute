package edu.hku.cs

/**
  * Created by jianyu on 3/4/17.
  */

import edu.columbia.cs.psl.phosphor.runtime.Tainter

/**
  * Created by jianyu on 3/3/17.
  */

class TypeTainter(ruleCollector: RuleCollector) {

  private val TAINT_START_KEY = 1
  private val TAINT_START_VALUE = 0x4fffffff + 1

  /* Taint key from 1 */
  private var _currentTaintKey = TAINT_START_KEY

  private var _currentTaintValue = TAINT_START_VALUE

  private var _currentPrint = 1

  private var deps : List[Tuple2[Int, List[Int]]] = _

  /* Taint value from 2^32 */

  private def currentTaintKey(): Integer = {
    if (_currentTaintKey >= _currentTaintValue)
      return 0
    val returnTaint = _currentTaintKey
    _currentTaintKey <<= 1
    returnTaint
  }

  private def currentTaintValue(): Integer = {
    /*    if (_currentTaintKey >= _currentTaintValue)
          return 0
        val returnTaint = _currentTaintValue
        _currentTaintValue >>= 1
        returnTaint*/
    currentTaintKey()
  }

  private def currentPrint(): Int = {
    val returnCurrent = _currentPrint
    _currentPrint += 1
    returnCurrent
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

      /* Primitive */
      case int: Int => Tainter.taintedInt(int, currentTaintKey()).asInstanceOf[T]
      case long: Long => Tainter.taintedLong(long, currentTaintKey()).asInstanceOf[T]
      case short: Short => Tainter.taintedShort(short, currentTaintKey()).asInstanceOf[T]
      case float: Float => Tainter.taintedFloat(float, currentTaintKey()).asInstanceOf[T]
      case double: Double => Tainter.taintedDouble(double, currentTaintKey()).asInstanceOf[T]
      case bool: Boolean => Tainter.taintedBoolean(bool, currentTaintKey()).asInstanceOf[T]
      case char: Char => Tainter.taintedChar(char, currentTaintKey()).asInstanceOf[T]
      case byte: Byte => Tainter.taintedByte(byte, currentTaintKey()).asInstanceOf[T]
      /* Primitive Array */
      case int: Array[Int] => Tainter.taintedIntArray(int, currentTaintValue()).asInstanceOf[T]
      case long: Array[Long] => Tainter.taintedLongArray(long, currentTaintValue()).asInstanceOf[T]
      case short: Array[Short] => Tainter.taintedShortArray(short, currentTaintValue()).asInstanceOf[T]
      case float: Array[Float] => Tainter.taintedFloatArray(float, currentTaintValue()).asInstanceOf[T]
      case double: Array[Double] => Tainter.taintedDoubleArray(double, currentTaintValue()).asInstanceOf[T]
      case bool: Array[Boolean] => Tainter.taintedBooleanArray(bool, currentTaintValue()).asInstanceOf[T]
      case char: Array[Char] => Tainter.taintedCharArray(char, currentTaintValue()).asInstanceOf[T]
      case byte: Array[Byte] => Tainter.taintedByteArray(byte, currentTaintValue()).asInstanceOf[T]
      /* Object */
      case obj: Object => obj.asInstanceOf[T]
    }
  }

  /* Clear all taint in the object, using taintAll may be fine????? */
  private def printTaintHelper[T](obj: T): T = {
    obj match {
      case product: Product => product.productIterator.foreach(printTaintHelper)
      case v: Int => deps = (currentPrint(), decomposeTaint(Tainter.getTaint(v))) :: deps
      case v: Int =>  deps = (currentPrint(), decomposeTaint(Tainter.getTaint(v))) :: deps
      case v: Short => deps = (currentPrint(), decomposeTaint(Tainter.getTaint(v))) :: deps
      case v: Long => deps = (currentPrint(), decomposeTaint(Tainter.getTaint(v))) :: deps
      case v: Double => deps = (currentPrint(), decomposeTaint(Tainter.getTaint(v))) :: deps
      case v: Float => deps = (currentPrint(), decomposeTaint(Tainter.getTaint(v))) :: deps
      case v: Byte => deps = (currentPrint(), decomposeTaint(Tainter.getTaint(v))) :: deps
      case v: Char => deps = (currentPrint(), decomposeTaint(Tainter.getTaint(v))) :: deps
      case v: Boolean => deps = (currentPrint(), decomposeTaint(Tainter.getTaint(v))) :: deps
      case v: Object => deps = (currentPrint(), decomposeTaint(Tainter.getTaint(v))) :: deps
    }
    obj
  }

  private def decomposeTaint(tag: Int): List[Int] = {
    var seq = List[Int]()
    for (i <- 0 until 30) {
      if ((tag & (1 << i)) != 0)
        seq = (i + 1) :: seq
    }
    seq
  }

  def getTaint[T](obj: T): T = {
    _currentPrint = 1
    deps = List()
    val returnVal = printTaintHelper(obj)
    ruleCollector.addRule(deps)
    returnVal
  }

  def setTaint[T](obj: T): T = {
    _currentTaintKey = TAINT_START_KEY
    _currentTaintValue = TAINT_START_VALUE
    taintAllHelper(obj)
  }
}

object TypeTainter{

  def f(a: (Int, Int, Int)): (Int, Int, Int) = {
    (a._1 + a._2, a._1 + a._3, a._2 + a._3)
  }

  def main(args: Array[String]): Unit = {
    val ruleCollector = RuleManager.managerInstance(0).collectorInstance(1)
    val typeTainter = new TypeTainter(ruleCollector)
    var a = (1, 2, 3)
    var k = (1, 3, 2)
    val c = typeTainter.setTaint(a)
    val g = typeTainter.setTaint(k)
    val d = f(c)
    typeTainter.getTaint(d)
    typeTainter.getTaint(d)
    RuleManager.managerInstance(0).collect()
    val b = (2,3)
  }
}
