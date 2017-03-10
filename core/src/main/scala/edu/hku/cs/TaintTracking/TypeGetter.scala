package edu.hku.cs.TaintTracking

import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}
/**
  * Created by jianyu on 3/10/17.
  */

object TypeGetter {

  def getTypeTag(obj: Any): String = {
    getTypeHelper(obj)
  }

  def getTypeHelper[T: ClassTag](obj: T): String = {
    obj match {
      case (_1, _2) => (getTypeHelper(_1), getTypeHelper(_2)).toString()
      case (_1, _2, _3) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3)).toString()
      case (_1, _2, _3, _4) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3), getTypeHelper(_4)).toString()
      case (_1, _2, _3, _4, _5) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3), getTypeHelper(_4), getTypeHelper(_5)).toString()
      case (_1, _2, _3, _4, _5, _6) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3), getTypeHelper(_4), getTypeHelper(_5), getTypeHelper(_6)).toString()
      case (_1, _2, _3, _4, _5, _6, _7) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3), getTypeHelper(_4), getTypeHelper(_5), getTypeHelper(_6), getTypeHelper(_7)).toString()
      case (_1, _2, _3, _4, _5, _6, _7, _8) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3), getTypeHelper(_4), getTypeHelper(_5), getTypeHelper(_6), getTypeHelper(_7), getTypeHelper(_8)).toString()
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3), getTypeHelper(_4), getTypeHelper(_5), getTypeHelper(_6), getTypeHelper(_7), getTypeHelper(_8), getTypeHelper(_9)).toString()
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3), getTypeHelper(_4), getTypeHelper(_5), getTypeHelper(_6), getTypeHelper(_7), getTypeHelper(_8), getTypeHelper(_9), getTypeHelper(_10)).toString()
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3), getTypeHelper(_4), getTypeHelper(_5), getTypeHelper(_6), getTypeHelper(_7), getTypeHelper(_8), getTypeHelper(_9), getTypeHelper(_10), getTypeHelper(_11)).toString()
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3), getTypeHelper(_4), getTypeHelper(_5), getTypeHelper(_6), getTypeHelper(_7), getTypeHelper(_8), getTypeHelper(_9), getTypeHelper(_10), getTypeHelper(_11), getTypeHelper(_12)).toString()
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3), getTypeHelper(_4), getTypeHelper(_5), getTypeHelper(_6), getTypeHelper(_7), getTypeHelper(_8), getTypeHelper(_9), getTypeHelper(_10), getTypeHelper(_11), getTypeHelper(_12), getTypeHelper(_13)).toString()
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3), getTypeHelper(_4), getTypeHelper(_5), getTypeHelper(_6), getTypeHelper(_7), getTypeHelper(_8), getTypeHelper(_9), getTypeHelper(_10), getTypeHelper(_11), getTypeHelper(_12), getTypeHelper(_13), getTypeHelper(_14)).toString()
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3), getTypeHelper(_4), getTypeHelper(_5), getTypeHelper(_6), getTypeHelper(_7), getTypeHelper(_8), getTypeHelper(_9), getTypeHelper(_10), getTypeHelper(_11), getTypeHelper(_12), getTypeHelper(_13), getTypeHelper(_14), getTypeHelper(_15)).toString()
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3), getTypeHelper(_4), getTypeHelper(_5), getTypeHelper(_6), getTypeHelper(_7), getTypeHelper(_8), getTypeHelper(_9), getTypeHelper(_10), getTypeHelper(_11), getTypeHelper(_12), getTypeHelper(_13), getTypeHelper(_14), getTypeHelper(_15), getTypeHelper(_16)).toString()
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3), getTypeHelper(_4), getTypeHelper(_5), getTypeHelper(_6), getTypeHelper(_7), getTypeHelper(_8), getTypeHelper(_9), getTypeHelper(_10), getTypeHelper(_11), getTypeHelper(_12), getTypeHelper(_13), getTypeHelper(_14), getTypeHelper(_15), getTypeHelper(_16), getTypeHelper(_17)).toString()
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3), getTypeHelper(_4), getTypeHelper(_5), getTypeHelper(_6), getTypeHelper(_7), getTypeHelper(_8), getTypeHelper(_9), getTypeHelper(_10), getTypeHelper(_11), getTypeHelper(_12), getTypeHelper(_13), getTypeHelper(_14), getTypeHelper(_15), getTypeHelper(_16), getTypeHelper(_17), getTypeHelper(_18)).toString()
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3), getTypeHelper(_4), getTypeHelper(_5), getTypeHelper(_6), getTypeHelper(_7), getTypeHelper(_8), getTypeHelper(_9), getTypeHelper(_10), getTypeHelper(_11), getTypeHelper(_12), getTypeHelper(_13), getTypeHelper(_14), getTypeHelper(_15), getTypeHelper(_16), getTypeHelper(_17), getTypeHelper(_18), getTypeHelper(_19)).toString()
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3), getTypeHelper(_4), getTypeHelper(_5), getTypeHelper(_6), getTypeHelper(_7), getTypeHelper(_8), getTypeHelper(_9), getTypeHelper(_10), getTypeHelper(_11), getTypeHelper(_12), getTypeHelper(_13), getTypeHelper(_14), getTypeHelper(_15), getTypeHelper(_16), getTypeHelper(_17), getTypeHelper(_18), getTypeHelper(_19), getTypeHelper(_20)).toString()
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3), getTypeHelper(_4), getTypeHelper(_5), getTypeHelper(_6), getTypeHelper(_7), getTypeHelper(_8), getTypeHelper(_9), getTypeHelper(_10), getTypeHelper(_11), getTypeHelper(_12), getTypeHelper(_13), getTypeHelper(_14), getTypeHelper(_15), getTypeHelper(_16), getTypeHelper(_17), getTypeHelper(_18), getTypeHelper(_19), getTypeHelper(_20), getTypeHelper(_21)).toString()
      case (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22) => (getTypeHelper(_1), getTypeHelper(_2), getTypeHelper(_3), getTypeHelper(_4), getTypeHelper(_5), getTypeHelper(_6), getTypeHelper(_7), getTypeHelper(_8), getTypeHelper(_9), getTypeHelper(_10), getTypeHelper(_11), getTypeHelper(_12), getTypeHelper(_13), getTypeHelper(_14), getTypeHelper(_15), getTypeHelper(_16), getTypeHelper(_17), getTypeHelper(_18), getTypeHelper(_19), getTypeHelper(_20), getTypeHelper(_21), getTypeHelper(_22)).toString()
      case _ => obj.getClass.toString
    }
  }
}