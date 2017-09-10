package edu.hku.cs.dft.tracker

import java.io.ObjectOutputStream
import java.lang.reflect.Method

import edu.columbia.cs.psl.phosphor.{Configuration, SelectiveInstrumentationManager}
import edu.hku.cs.dft.datamodel.DataModel
import edu.hku.cs.dft.network.NettyEndpoint

/**
  * Created by jianyu on 9/7/17.
  */
trait LocalChecker extends NettyEndpoint with Serializable{

  /* instrument some functions (I/O funcs) */
  def instrument(): Unit

  def addMethod(className: String, name: String, desc: String): Unit = {
    SelectiveInstrumentationManager.addMethod(className, name, desc)
  }

}
