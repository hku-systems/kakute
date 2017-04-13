package edu.hku.cs.dft.debug

/**
  * Created by jianyu on 4/13/17.
  */


// store the latest info of fail task
class DebugStorage(val stageId: Int, val partitionId: Int) {

  private var latest: Any = _

  def push[T](o: T): Unit = {
    latest = o
  }

  def pop(): Any = latest

}

object DebugStorage {

  private var storage: Map[Long, DebugStorage] = Map()

  private var threadLocalStorage: ThreadLocal[DebugStorage] = _

  def newInstance(stage: Int, partition: Int): DebugStorage = {
    val ds = new DebugStorage(stage, partition)
    val id = Thread.currentThread().getId
    threadLocalStorage = new ThreadLocal[DebugStorage]() {
      override def initialValue(): DebugStorage = ds
    }
    ds
  }

  def getInstance(): DebugStorage = {
    threadLocalStorage.get()
  }

}
