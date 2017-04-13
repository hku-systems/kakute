package edu.hku.cs.dft.debug

/**
  * Created by jianyu on 4/13/17.
  */
object DebugTracer {

  private var threadLocalStorage: ThreadLocal[DebugStorage] = _

  def newStorage(stage: Int, partition: Int): DebugStorage = {
    threadLocalStorage = new ThreadLocal[DebugStorage]() {
      override def initialValue(): DebugStorage = new DebugStorage(stage, partition)
    }
    threadLocalStorage.get()
  }

  def getInstance(): DebugStorage = {
    threadLocalStorage.get()
  }

  def trace[T](o: T): T = {
    val debugStorage = getInstance()
    debugStorage.push(o)
    o
  }

  def backTrace(): Any = getInstance().pop()

}
