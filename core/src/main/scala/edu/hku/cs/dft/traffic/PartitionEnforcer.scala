package edu.hku.cs.dft.traffic

/**
  * Created by jianyu on 3/24/17.
  */

/**
  * A [[PartitionEnforcer]] is use to enfore the partition scheme infer from
  * the profiling run to the real one
  *
  * Every time a new data model is created, the data model will find out if
  * there are any policy and its corresponding partitioner here([[getPartitioner]])
*/

class PartitionEnforcer(collection: PartitionSchemes.PartitionSchemeCollection) {

  val partitionSchemes: Map[String, PartitionScheme] = collection

  var partitionerMapping: Map[String, DependentPartitioner] = Map()

  def getPartitioner(num: Int, variableId: String): DependentPartitioner = {
    val id = variableId + num
    // get the partitioner, or construct it when it is not ready
    partitionerMapping.getOrElse(id, {
      val scheme = partitionSchemes.getOrElse(id, null)
      if (scheme != null) {
        val d = new DependentPartitioner(num ,scheme.hashKeySet)
        partitionerMapping += id -> d
        d
      } else {
        null
      }
    })
  }



}
