package edu.hku.cs.dft.traffic

import edu.hku.cs.dft.optimization.RuleMaker

/**
  * Created by max on 22/3/2017.
  */

/**
  * A [[DependentTagger]] use dependent key as the hash key, and add
  * this key to all the mapper
*/

class DependentTagger extends PartitionSchemeTagger{

  // PartitionTags

  override def tagScheme(): Unit = {
    val checkList = this.shuffleSet
    checkList.foreach(v => {

      // tag from reducer to the root
      var currentData = List(v)
      var data = dataSet(v)
      var currentPartitionTags = partitionTags.getOrElse(v, List())
      currentPartitionTags = PartitionScheme(RuleMaker.typeInfoLength(data.dataType),
        (0 to data.reduceKeyRange).toSet) :: currentPartitionTags
      // tag all partition scheme to the top
      while(currentData.nonEmpty) {
        val v = currentData.last
        val data = this.dataSet(v)
        data.deps.foreach(d => {
          val depKeys = Set()

          // here, we only choose the most important key, put it into the list

          currentData = d._1 :: currentData
        })
        currentData = currentData.init
      }

    })
  }

}
