package edu.hku.cs.dft.traffic

import edu.hku.cs.dft.optimization.Analyzer

/**
  * Created by max on 22/3/2017.
  */

/**
  * a [[PartitionSchemeTagger]] add partition scheme tag to a data model
  * a [[PartitionScheme]] consists of partition keySet(set of key index)
  * and the total number of keys that a data model have. A [[PartitionScheme]]
  * also have a r value to represent how strong this scheme will be
*/


case class PartitionScheme(keyCount: Int, hashKeySet: Set[Int])

abstract class PartitionSchemeTagger extends Analyzer {

  // generated partition scheme
  var partitionTags: Map[String, List[PartitionScheme]] = Map()

  def tagScheme(): Unit = _

}