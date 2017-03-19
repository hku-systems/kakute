package edu.hku.cs.Optimization

/**
  * Created by max on 18/3/2017.
  */
object SymbolManager {

  var scopt: String = ""

  private var id: Int = 0
  private var StringId: Map[String, Int] = Map()

  def newId(): Int = {
    id += 1
    id
  }

  def setAndGet(string: String): Int = StringId.getOrElse(string, newId())
}
