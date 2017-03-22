package edu.hku.cs.dft.traffic

import edu.hku.cs.dft.optimization.RuleMaker

/**
  * Created by max on 22/3/2017.
  */

/**
  * A [[DependentTagger]] use dependent key as the hash key, and add
  * this key to all the mapper
  * it use data count as r value in the implementation
*/

class DependentTagger extends PartitionSchemeTagger{

  // PartitionTags

  override def tagScheme(): Unit = {
    val checkList = this.shuffleSet
    checkList.foreach(clist => {

      // tag from reducer to the root
      var currentDatas = List(clist)
      var ldata = dataSet(clist)

      var thisTags: Map[String, PartitionScheme] = Map()

      // we use dataCount as r
      val currentScheme = PartitionScheme(RuleMaker.typeInfoLength(ldata.dataType),
        (1 to ldata.reduceKeyRange).toSet, ldata.dataCount)
      thisTags += clist -> currentScheme

      // tag all partition scheme to the top
      while(currentDatas.nonEmpty) {
        val currentValue = currentDatas.last
        val currentData = this.dataSet(currentValue)
        currentData.deps.foreach(dep => {
          // find the dependent keyset
          dep._2.foreach(rule => {
            val mapDep = rule._1.toMap
            val datar = dataSet(dep._1)
            val reduceSet = partitionTags.getOrElse(currentValue, Set())
            // if it is null, then it came across a problem
            assert(reduceSet.nonEmpty)
            // add according to the current rule
            reduceSet.foreach(c => {
              var depKeys: Set[Int] = Set()
              c.hashKeySet.foreach(k => {
                if (mapDep.contains(k)) {
                  depKeys ++= mapDep(k).toSet
                }
              })
              val currentScheme = PartitionScheme(RuleMaker.typeInfoLength(dataSet(dep._1).dataType),
                depKeys, datar.dataCount)
              thisTags += dep._1 -> currentScheme
            })
          })
          currentDatas = dep._1 :: currentDatas
        })
        currentDatas = currentDatas.init
      }
    })
  }
}
