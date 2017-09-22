package edu.hku.cs.dft.tracker

import java.io.ObjectOutputStream
import java.lang.reflect.Method

import edu.columbia.cs.psl.phosphor.runtime.{Taint, TaintChecker}
import edu.columbia.cs.psl.phosphor.{Configuration, SelectiveInstrumentationManager}
import edu.hku.cs.dft.datamodel.DataModel
import edu.hku.cs.dft.network.NettyEndpoint

/**
  * Created by jianyu on 9/7/17.
  */
trait LocalChecker extends NettyEndpoint with Serializable{

  /* checker function for int */
  val localCheckerFunc_int: Option[(Int => Unit)] = None

  /* checker function for obj */
  val localCheckerFunc_obj: Option[Object => Unit] = None

  // set checker function
  def setCheckerFunc(): Unit = {

    if (localCheckerFunc_int.isDefined) {
      TaintChecker.setCheckerFunc(new TaintChecker.CheckFuncInt {
        override def checkTag(i: Int): Unit = localCheckerFunc_int.get(i)
      })
    }

    if (localCheckerFunc_obj.isDefined) {
      TaintChecker.setCheckerFunc(new TaintChecker.CheckFuncObj {
        override def checkTag(o: Object): Unit = localCheckerFunc_obj.get(o)
      })
    }

  }

}
