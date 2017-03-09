package edu.hku.cs.DataModel

import edu.hku.cs.DataModel.DataOperation.DataOperation
import org.apache.spark.rdd.RDD
import scala.reflect.runtime.{universe => ru}
/**
  * Created by jianyu on 3/5/17.
  */

/**
 *  A [[PlatformHandle]] do ?
 *  To support a new system, things developers should do is to
 *  override PlatformHandle to support different platform and
 *  change the entry point in [[GraphManager]]
 */

trait PlatformHandle {
  def frameworkId(): Int
  def fathers(): List[PlatformHandle]
  def typeInto(): String
  def op(): DataOperation
  def variable(): String
}

class SparkPlatformHandle(frameworkR: RDD[_]) extends PlatformHandle {
  override def frameworkId(): Int = frameworkR.id

  override def typeInto(): String = {
    frameworkR.elementTypeTag()
  }

  def getTypeTag[T: ru.TypeTag](obj: T): ru.TypeTag[T] = ru.typeTag[T]

  override def fathers(): List[PlatformHandle] = {
    var fa: List[PlatformHandle] = List()
    frameworkR.dependencies.foreach(dep => {
      fa = new SparkPlatformHandle(dep.rdd) :: fa
    })
    fa
  }

  override def op(): DataOperation = {
    null
  }

  override def variable(): String = {
    frameworkR.name
  }
}