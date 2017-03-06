package edu.hku.cs.DataModel

import edu.hku.cs.DataModel.DataOperation.DataOperation
import edu.hku.cs.Optimization.RuleCollector
import edu.hku.cs.Optimization.RuleCollector.RuleSet

/**
  * Created by jianyu on 3/5/17.
  */

object DataOperation extends Enumeration {
  type DataOperation = Value
  val Map, Reduce, Union, Collect = Value
}

class DataModel(id: Long, op: DataOperation,
                        frameworkHandle: PlatformHandle, variableId: String) {

  var isOrigin = false

  private var _fathers: List[DataModel] = List()

  private var _sons: List[DataModel] = _

  private var _deps: Map[DataModel, RuleSet] = _

  override def equals(obj: Any): Boolean = {
    obj match {
      case dataModel: DataModel => dataModel.id == this.id
      case _ => false
    }
  }

  def sons(): List[DataModel] = _sons

  def addDeps(ruleSet: RuleSet, dataModel: DataModel): Unit = {
    if (_deps == null)
      _deps = Map()
    val found = _deps.find(_._1 == dataModel)
    val returnDeps = if (found.isEmpty) {
      dataModel -> ruleSet
    } else {
      // Combine two RuleSet
      dataModel -> RuleCollector.CombineRule(ruleSet, found.get._2)
    }
    _deps += returnDeps
  }

  def addFathers(data: DataModel): Unit = {
    _fathers = data :: _fathers
  }

  def addSon(dataModel: DataModel): Unit = {
    _sons = dataModel :: _sons
  }

  /* Find the origin data model */
  def origin(): DataModel = {
    null
  }

  def frameworkId(): Int = frameworkHandle.frameworkId()

}
