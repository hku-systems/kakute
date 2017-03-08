package edu.hku.cs.DataModel

import edu.hku.cs.Optimization.RuleCollector.RuleSet
import org.apache.spark.rdd.RDD

/**
  * Created by jianyu on 3/5/17.
  */

/* This class manage the graph creation
 * Create / Update Data Model
 * Travel the graph when one task trigger
 *
 * T is Platform reference */

class GraphManager {

  // Create EndPoint here to communicate with the client

  var frameworkIdMapData: Map[Int, DataModel] = Map()

  var rootData: List[DataModel] = List()



  private var _currentId: Long = 0

  def currentId(): Long = {
    val returnId = _currentId
    _currentId += 1
    returnId
  }

  def spark_entry(rDD: RDD[_]) {
    entry(new SparkPlatformHandle(rDD))
  }

  /* trigger this function when one task is added */
  def entry(platformHandle: PlatformHandle): Unit = {
    getOrElseCreate(platformHandle)
  }

  def getOrElseCreate(platformHandle: PlatformHandle): DataModel = {
    val found = frameworkIdMapData.find(_._1 == platformHandle.frameworkId())
    val returnVal = if (found.isEmpty) {
      val data = new DataModel(currentId(),
        platformHandle.op(), platformHandle, null)
      val fathersHandle = platformHandle.fathers()
      if (fathersHandle.isEmpty) {
        data.isOrigin = true
        rootData = data :: rootData
      } else {
        fathersHandle.foreach(fatherHandle => {
          val father = getOrElseCreate(fatherHandle)
          father.addSon(data)
          data.addFathers(father)
        }
        )
      }
      data
    } else {
      found.get._2
    }
    frameworkIdMapData += platformHandle.frameworkId() -> returnVal
    returnVal
  }

  def getDatamodelOrThrow(platformHandle: PlatformHandle): DataModel = {
    val found = frameworkIdMapData.find(_._1 == platformHandle.frameworkId())
    if (found.isEmpty) {
      throw DataNotFoundException(platformHandle.frameworkId())
    }
    found.get._2
  }

  def addTrackingRule(platformHandle: PlatformHandle, depDataHandle: PlatformHandle,ruleSet: RuleSet): Unit = {
    val ruleData = getDatamodelOrThrow(platformHandle)
    val depData = getDatamodelOrThrow(depDataHandle)
    ruleData.addDeps(ruleSet, depData)
  }

  case class DataNotFoundException(i: Int) extends Exception

}

object GraphManager{
  val graphManager = new GraphManager

}
