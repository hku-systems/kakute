package edu.hku.cs.dft.optimization

import edu.hku.cs.dft.datamodel.DataOperation.DataOperation
import edu.hku.cs.dft.datamodel.{DataModel, DataOperation, GraphManager, PlatformHandle}
import edu.hku.cs.dft.optimization.RuleCollector.RuleSet
import edu.hku.cs.dft.tracker.DFTUtils

/**
  * Created by jianyu on 3/16/17.
  */

class LoopReducedDataModel(platformHandle: PlatformHandle, variable: String) {

  var modelSet: Set[DataModel] = Set()

  var dataType: Any = _

  var count: Int = 0

  var deps: Map[String, RuleSet] = Map()

  def op(): DataOperation = platformHandle.op()

  def addModel(dataModel: DataModel, fatherModel: DataModel):this.type = {
    modelSet += dataModel
    count += 1
    if (dataType == null)
      dataType = dataModel.dataType()
    dataModel.deps().foreach(dp => {
      val dpName = if (dp._1.name() == dataModel.name()) fatherModel.name() else dp._1.name()
      if (deps.contains(dpName)) {
        deps += dpName -> RuleCollector.CombineRule(dp._2, deps(dpName))
      } else {
        deps += dpName -> dp._2
      }
    })

    /**
      * fill the legacy data with empty entry
    */

    dataModel.fathers().foreach(fa => {
      if (!deps.contains(fa.name())) {
        deps += fa.name() -> Map()
      }
    })
    this
  }

  override def toString: String = {
    val stringBuilder = new StringBuilder
    stringBuilder.append(s"[$variable] $dataType -> { ")
    modelSet.foreach(d => {
      stringBuilder.append(d.ID)
      stringBuilder.append(" ")
    })
    stringBuilder.append(s"}($count) $op deps: ")
    deps.foreach(r => {
      stringBuilder.append(r._1 + " -> ")
      stringBuilder.append(r._2)
      stringBuilder.append(" ;")
    })
    stringBuilder.toString()
  }
}


class Analyser {

  private val prefixRoot: String = "Root-"

  private var rootCurrent: Int = 1

  // String to String generation
  private var setMap: Map[String, Set[String]] = Map()

  // String to set of dataModel, they are the same
  private var dataSet: Map[String, LoopReducedDataModel] = Map()

  private var rootData: List[String] = List()

  private var nullNum = 1

  private var visitedSet: Set[Int] = Set()

  def entry(graphManager: GraphManager): Unit = {
    var dumpList = graphManager.rootData
    dumpList.foreach(dump => {
      if (!DFTUtils.nameValid(dump.name())) {
        dump.setName(prefixRoot + rootCurrent)
        rootCurrent += 1
      }
      rootData = dump.name() :: rootData
      entryHelper(dump, null)
    })
  }

  def entryHelper(dataModel: DataModel, fatherModel: DataModel): Unit = {
    // if the relation between this two datamodel name is missing,
    // then create the relation
    // if the relation exists,
    // then check if they have the same kind of child, and chang its current
    // name when they are different
    if (!DFTUtils.nameValid(dataModel.name())) {
      dataModel.setName("null" + nullNum)
      nullNum += 1
    }
    if (fatherModel != null) {
      if (setMap.contains(fatherModel.name())) {
        setMap += fatherModel.name() -> (setMap(fatherModel.name()) + dataModel.name())
      } else {
        setMap += fatherModel.name() -> Set[String](dataModel.name())
      }
    }
    if (visitedSet.contains(dataModel.ID.toInt))
      return

    dataSet += dataModel.name() -> dataSet.getOrElse(dataModel.name(), new LoopReducedDataModel(dataModel.handle(), dataModel.name())).addModel(dataModel, fatherModel)
    visitedSet += dataModel.ID.toInt
    dataModel.sons().foreach(t => {
      entryHelper(t, dataModel)
    })

  }

  /**
    * In the [[firstRoundEntry]], we will add the dependency info that may be hard or unnecessary
    * to infer
    * TODO: Now we only consider union, but we actually may use the infomation like union to provide
    * a higher-level information to optimize the system
    *
  */
  def firstRoundEntry(): Unit = {
    var checkList: List[String] = rootData
    while(checkList.nonEmpty) {
      val v = checkList.last
      if (dataSet(v).deps.isEmpty) {
        dataSet(v).op() match {
          case DataOperation.Union => {
            dataSet(v)
          }
        }
      }
    }

  }

  def dump(): Unit = {
    var startStrings: List[String] = rootData
    var dumpSet: Set[String] = Set()
    while (startStrings.nonEmpty) {
      val v = startStrings.last
      if (!dumpSet.contains(v)) {
        print(dataSet(v))
        dumpSet += v
        print(" ===>>> ")
        if (setMap.contains(v)) {
          setMap(v).foreach(k => {
            startStrings = k :: startStrings
            print(k + " ")
          })
        }
        println()
      }
      startStrings = startStrings.init
    }
  }

  // print the graph when exit
  def exitPoint(): Unit = {
    dump()
  }

}
