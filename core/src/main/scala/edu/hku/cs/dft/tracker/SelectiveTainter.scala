package edu.hku.cs.dft.tracker

/**
  * Created by jianyu on 3/7/17.
  */

/**
  * A [[SelectiveTainter]] add to taint to a specific record,
  * when a record fullfill some conditions
  * The default rule is in the rule
  * So 0 -> default_func
*/

class SelectiveTainter(filter: Map[Int, Any => Any], defaultTag: Any = 0) extends BaseTainter{

  private var _index = 0

  private var _indexDeps = 0

  private var _deps: Map[Int, CombinedTaint[_]] = Map()

  private var _positionFilter: Map[Int, Any => Any] = if (filter == null) Map() else filter

  private val tainter = if (TainterHandle.trackingTaint == TrackingTaint.ObjTaint) new ObjectTainter else new IntTainter

  // if there are no rule for default, just make then untainted
  def defaultFilter: Any => Any = _positionFilter.getOrElse(0, _ => defaultTag)

  def setTaintWithTaint[T](obj: T, filter: Map[Int, Any]): T = {
    setFilter(DFTUtils.markPositionToMap(filter))
    setTaint(obj)
  }

  def setFilter(filter: Map[Int, Any => Any]): SelectiveTainter = {
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
  private def taintTupleOne[T](obj: T, tag: Any): T = {
    // TODO check if tag is bypassed
    if (tag == -1) {
      obj
    } else {
      val r = obj match {
        case v: Int => tainter.setTaint(v, tag)
        case v: Long => tainter.setTaint(v, tag)
        case v: Double => tainter.setTaint(v, tag)
        case v: Float => tainter.setTaint(v, tag)
        case v: Short => tainter.setTaint(v, tag)
        case v: Boolean => tainter.setTaint(v, tag)
        case v: Byte => tainter.setTaint(v, tag)
        case v: Char => tainter.setTaint(v, tag)
        case arr: Array[_] => tainter.setTaint(arr, tag)
        case it: Iterator[Any] => it.map(t => taintTupleOne(t, tag)).asInstanceOf[T]
        case it: Iterable[Any] => it.map(t => taintTupleOne(t, tag)).asInstanceOf[T]
        case ob: Object => tainter.setTaint(ob, tag)
      }
      r.asInstanceOf[T]
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
      // Can be any obj
      // if (!taint.isInstanceOf[Int] || !taint.isInstanceOf[Integer]) throw new Exception("type not match")
      val tag = taint
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
          taintTupleOne(obj, taint)
      }
    }

  }

  override def setTaint[T](obj: T): T = {
    _index = 0
    val k = taintAllHelper(obj)
    k
  }

  def getTupleTaint[T](obj: T): Any = {
    obj match {
      case (_1, _2) => (getTupleTaint(_1), getTupleTaint(_2))
      case (_1, _2, _3) => (getTupleTaint(_1), getTupleTaint(_2), getTupleTaint(_3))
      case _ => getTupleTaintOne(obj)
    }
  }

  def getTupleTaintOne[T](obj: T): Any = {
    obj match {
      case v: Int => tainter.getTaint(v)
      case v: Long => tainter.getTaint(v)
      case v: Double => tainter.getTaint(v)
      case v: Float => tainter.getTaint(v)
      case v: Short => tainter.getTaint(v)
      case v: Boolean => tainter.getTaint(v)
      case v: Byte => tainter.getTaint(v)
      case v: Char => tainter.getTaint(v)
        // we should show the taint of the all object
        // also array should be consider
      case v: Iterable[_] => v.map(getTupleTaintOne).toList
      case v: Iterator[_] => v.map(getTupleTaintOne).toList
      case v: Array[_] => v.map(getTupleTaint).toList
      case v: Object => tainter.getTaint(v)
      case null => null
      case _ => throw new Exception("type mismatch type " + obj.getClass.getSimpleName)
    }
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
      val r = obj match {
        // Primitive
        case v: Int => tainter.setTaint(v, tag)
        case v: Long => tainter.setTaint(v, tag)
        case v: Double => tainter.setTaint(v, tag)
        case v: Float => tainter.setTaint(v, tag)
        case v: Short => tainter.setTaint(v, tag)
        case v: Boolean => tainter.setTaint(v, tag)
        case v: Byte => tainter.setTaint(v, tag)
        case v: Char => tainter.setTaint(v, tag)
        case arr: Array[_] => tainter.setTaint(arr, tag)
        case it: Iterator[Any] => it.map(t => taintTupleOne(t, tag)).asInstanceOf[T]
        case it: Iterable[Any] => it.map(t => taintTupleOne(t, tag)).asInstanceOf[T]
        case ob: Object => tainter.setTaint(ob, tag)
        case _ => obj
      }
      r.asInstanceOf[T]
    }
  }

  def getOne[T](obj: T): T = {
    _indexDeps += 1
    val tag = obj match {
      case v: Int => tainter.getTaint(v)
      case v: Long => tainter.getTaint(v)
      case v: Double => tainter.getTaint(v)
      case v: Float => tainter.getTaint(v)
      case v: Short => tainter.getTaint(v)
      case v: Boolean => tainter.getTaint(v)
      case v: Byte => tainter.getTaint(v)
      case v: Char => tainter.getTaint(v)
      case ob: Object => tainter.getTaint(ob)
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

  override def getTaintList(obj: Any): Map[Int, CombinedTaint[_]] = {
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