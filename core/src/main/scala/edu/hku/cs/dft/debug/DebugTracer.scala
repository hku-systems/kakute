package edu.hku.cs.dft.debug

/**
  * Created by jianyu on 4/13/17.
  */
object DebugTracer {

  def trace[T](o: T): T = {
    val debugStorage = DebugStorage.getInstance()
    debugStorage.push(o)
    o
  }

  def currentStack(): Unit = {
    val debugStorage = DebugStorage.getInstance()
    debugStorage.pop()
  }

}
