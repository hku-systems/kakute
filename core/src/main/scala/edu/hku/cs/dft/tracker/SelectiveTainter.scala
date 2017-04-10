package edu.hku.cs.dft.tracker
import edu.columbia.cs.psl.phosphor.runtime.Tainter

import scala.reflect.ClassTag

/**
  * Created by jianyu on 3/7/17.
  */

/**
  * A [[SelectiveTainter]] add to taint to a specific record,
  * when a record fullfill some conditions
  * The default rule is in the rule
  * So 0 -> default_func
*/

class SelectiveTainter(filter: Map[Int, Any => Int], defaultTag: Int = 0) extends BaseTainter{

  private var _index = 0

  private var _indexDeps = 0

  private var _deps: Map[Int, Int] = Map()

  private var _positionFilter: Map[Int, Any => Int] = if (filter == null) Map() else filter

  // if there are no rule for default, just make then untainted
  def defaultFilter: Any => Int = _positionFilter.getOrElse(0, _ => defaultTag)

  def setTaintWithTaint[T](obj: T, filter: Map[Int, Int]): T = {
    setFilter(DFTUtils.markPositionToMap(filter))
    setTaint(obj)
  }

  def setFilter(filter: Map[Int, Any => Int]): SelectiveTainter = {
    _positionFilter = filter
    this
  }

  /**
   * [[setTaintWithTupleTaint]] is a more flexible api to add taint, the tuple shuld be as the same
    * structure as the data: if obj is tuple2, then taint should be tuple2
  */
  def setTaintWithTupleTaint[T, M](obj: T, taint: M): T = {
    setTupleTaintHelper(obj, taint)
  }

  // TODO: Other Basic Collection of data ?
  private def taintTupleOne[T](obj: T, tag: Int): T = {
    if (tag < 0) {
      obj
    } else {
      obj match {
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
        case objs: Array[Object] => objs.foreach(obj => {
            Tainter.taintedObject(obj, tag)
          })
          obj
        case it: Iterator[Any] => it.map(t => taintTupleOne(t, tag)).asInstanceOf[T]
        case it: Iterable[Any] => it.map(t => taintTupleOne(t, tag)).asInstanceOf[T]
        case obj: Object =>
          Tainter.taintedObject(obj, tag)
          obj.asInstanceOf[T]
      }
    }
  }

  private def getBaseClass(obj: Any): (Boolean, Int) = {
    obj match {
      case p: Product => (true, p.productArity)
      case _ => (false, 0)
    }
  }

  private def setTupleTaintHelper[T, M](obj: T, taint: M): T = {
    if (getBaseClass(obj) != getBaseClass(taint)) {
      if (!taint.isInstanceOf[Int] || !taint.isInstanceOf[Integer]) throw new Exception("type not match")
      val tag = taint.asInstanceOf[Int]
      obj match {
        case (_1, _2) => (setTupleTaintHelper(_1, tag), setTupleTaintHelper(_2, tag)).asInstanceOf[T]
        case (_1, _2, _3) => (setTupleTaintHelper(_1, tag), setTupleTaintHelper(_2, tag), setTupleTaintHelper(_3, tag)).asInstanceOf[T]
        case (_1, _2, _3, _4) => (setTupleTaintHelper(_1, tag), setTupleTaintHelper(_2, tag), setTupleTaintHelper(_3, tag), setTupleTaintHelper(_4, tag)).asInstanceOf[T]
        case _ => taintTupleOne(obj, tag)
      }
    } else {
      obj match {
        case (_1, _2) => (setTupleTaintHelper(_1, taint.asInstanceOf[(_, _)]._1),
          setTupleTaintHelper(_2, taint.asInstanceOf[(_, _)]._2)).asInstanceOf[T]
        case (_1, _2, _3) => (setTupleTaintHelper(_1, taint.asInstanceOf[(_, _, _)]._1),
          setTupleTaintHelper(_2, taint.asInstanceOf[(_, _, _)]._2),
          setTupleTaintHelper(_3, taint.asInstanceOf[(_, _, _)]._3)).asInstanceOf[T]
        case (_1, _2, _3, _4) => (setTupleTaintHelper(_1, taint.asInstanceOf[(_, _, _, _)]._1),
          setTupleTaintHelper(_2, taint.asInstanceOf[(_, _, _, _)]._2),
          setTupleTaintHelper(_3, taint.asInstanceOf[(_, _, _, _)]._3),
          setTupleTaintHelper(_4, taint.asInstanceOf[(_, _, _, _)]._4)).asInstanceOf[T]
        case _ =>
          if (!taint.isInstanceOf[Int]) throw new Exception("no int taint")
          taintTupleOne(obj, taint.asInstanceOf[Int])
      }
    }

  }

  override def setTaint[T](obj: T): T = {
    _index = 0
    val k = taintAllHelper(obj)
    k
  }

  /**
    * if the the positionFilter is a f: Any => Int, if Int is positive, then tag will be added,
    * or if the tag is zero, then the tag will be clear.
    * if the tag is less than 0, then the tag will not change
    * TODO: Do we need this?
  */
  def taintOne[T](obj: T): T = {
    _index += 1
    val f = _positionFilter.getOrElse(_index, defaultFilter)
    val tag = f(obj)
    if (tag == -1) {
      obj
    } else {
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
        case objs: Array[Object] => objs.foreach(obj => {
            Tainter.taintedObject(obj, tag)
          })
          obj
        case it: Iterator[Any] => it.map(t => taintTupleOne(t, tag)).asInstanceOf[T]
        case it: Iterable[Any] => it.map(t => taintTupleOne(t, tag)).asInstanceOf[T]
        case obj: Object => {
          Tainter.taintedObject(obj, tag)
          obj.asInstanceOf[T]
        }
        case _ => obj
      }
    }
  }

  def getOne[T](obj: T): T = {
    _indexDeps += 1
    val tag = obj match {
      case v: Int => Tainter.getTaint(v)
      case v: Short => Tainter.getTaint(v)
      case v: Long => Tainter.getTaint(v)
      case v: Double => Tainter.getTaint(v)
      case v: Float => Tainter.getTaint(v)
      case v: Byte => Tainter.getTaint(v)
      case v: Char => Tainter.getTaint(v)
      case v: Boolean => Tainter.getTaint(v)
      case v: Object => Tainter.getTaint(v)
      case null => 0
      case _ => throw new Exception("type mismatch type " + obj.getClass.getSimpleName)
    }
    _deps += _indexDeps -> tag
    obj
  }

  private def taintAllHelper[T](obj: T): T = {
    obj match {
      /* Product, Scala Allow only 22 elements in a tuple */
      case arr: Array[Product] =>
        val markIndex = _index
        arr.map(ar => {
          _index = markIndex
          taintAllHelper(ar)
        }).copyToArray(arr)
        _index = markIndex
        arr.asInstanceOf[T]
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

  override def getTaintList(obj: Any): Map[Int, Int] = {
    _indexDeps = 0
    _deps = Map()
    getTaintHelper(obj)
    _deps
  }

  def getTaintHelper[T](obj: T): T = {
    obj match {
      case product: Product => product.productIterator.foreach(getTaintHelper)
      case _ => getOne(obj)
    }
    obj
  }

  override def getTaintAndReturn[T](obj: T): T = {
    throw new TaintException("Not implemented")
  }
}