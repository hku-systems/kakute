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

  var partitionTags: Map[String, Set[PartitionScheme]] = Map()

  var choosenSchemes: Map[String, PartitionScheme] = Map()

  override def tagScheme(): Unit = {
    val emptyScheme = PartitionScheme(0, Set(), 0)
    val checkList = this.shuffleSet
    checkList.foreach(clist => {

      // tag from reducer to the root
      var currentDatas = List(clist)
      var ldata = dataSet(clist)

      var thisTags: Map[String, Set[PartitionScheme]] = Map()

      // we use dataCount as r
      val currentScheme = PartitionScheme(RuleMaker.typeInfoLength(ldata.dataType),
        (1 to ldata.reduceKeyRange).toSet, ldata.dataCount)
      thisTags += clist -> Set(currentScheme)

      // tag all partition scheme to the top
      while(currentDatas.nonEmpty) {
        val currentValue = currentDatas.last
        val currentData = this.dataSet(currentValue)
        currentData.deps.foreach(dep => {
          // find the dependent keyset
          dep._2.foreach(rule => {
            val mapDep = rule._1.toMap
            val datar = dataSet(dep._1)
            val reduceSet = thisTags.getOrElse(currentValue, Set())
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

              var addSet = thisTags.getOrElse(dep._1, Set())
              val r = addSet.find(_ == depKeys).getOrElse(emptyScheme).r + datar.dataCount
              val currentScheme = PartitionScheme(RuleMaker.typeInfoLength(dataSet(dep._1).dataType),
                depKeys, r)
              addSet += currentScheme
              thisTags += dep._1 -> addSet
            })
          })
          currentDatas = dep._1 :: currentDatas
        })
        currentDatas = currentDatas.init
      }

      // combine the scheme to teh tag
      thisTags.foreach(tags => {
        var gotSchemes = partitionTags.getOrElse(tags._1, Set())
        val foundTag = gotSchemes.find(_ == currentScheme)
        tags._2.foreach(tag => {
          val addScheme: PartitionScheme = if (foundTag.nonEmpty) {
            PartitionScheme(tag.keyCount, tag.hashKeySet, tag.r + foundTag.get.r)
          } else {
            tag
          }
          gotSchemes += addScheme
        })
        partitionTags += tags._1 -> gotSchemes
      })

    })
  }

  /**
    * Here [[chooseScheme]] simply choose the smallest dependent set
  */
  override def chooseScheme(): Unit = {
    this.partitionTags.foreach(sp => {
      var scheme = sp._2.head
      sp._2.foreach(s => {
        if (s.hashKeySet.size < scheme.hashKeySet.size) {
          scheme = s
        }
      })
      choosenSchemes += sp._1 -> scheme
    })
  }

  def printScheme(): Unit = {
    this.choosenSchemes.foreach(t => {
      println(t._1 + " " + t._2)
    })
  }

}
