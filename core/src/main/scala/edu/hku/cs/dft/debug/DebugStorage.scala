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



}
