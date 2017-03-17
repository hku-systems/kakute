package edu.hku.cs.Optimization

import edu.hku.cs.DataModel.{DataModel, GraphManager}
import edu.hku.cs.TaintTracking
import edu.hku.cs.TaintTracking.DFTUtils

/**
  * Created by jianyu on 3/16/17.
  */

class DataSet(id: Int, name: String) {
  var dataModels: List[DataModel] = List()
  def append(dataModel: DataModel): Unit = {
    dataModels = dataModel :: dataModels
  }
}



class Analyser {


  private val prefixRoot: String = "Root-"

  private var rootCurrent: Int = 1

  // String to String generation
  private var setMap: Map[String, Set[String]] = Map()

  // String to set of datamodel, they are the same
  private var dataSet: Map[String, Set[DataModel]] = Map()

  private var nullNum = 1

  def entry(graphManager: GraphManager): Unit = {
    var dumpList = graphManager.rootData
    dumpList.foreach(dump => {
      if (!DFTUtils.nameValid(dump.name())) {
        dump.setName(prefixRoot + rootCurrent)
        rootCurrent += 1
      }
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
    dataSet += dataModel.name() -> (dataSet.getOrElse(dataModel.name(), Set[DataModel]()) + dataModel)
    dataModel.sons().foreach(t => {
      entryHelper(t, dataModel)
    })

  }

  def dump(): Unit = {
    setMap.foreach(s => {
      print("[" + s._1 + "] -> ")
      s._2.foreach(u => {
        val dataSetIds = new StringBuilder
        dataSet(u).foreach(k => {
          dataSetIds.append(k.ID)
          dataSetIds.append(",")
        })
        print(u + "[" + dataSetIds.toString() + "]")
      })
      println()
    })
  }

  // print the graph when exit
  def exitPoint(): Unit = {
    dump()
  }

}
