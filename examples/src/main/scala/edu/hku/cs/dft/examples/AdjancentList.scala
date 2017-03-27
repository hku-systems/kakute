package edu.hku.cs.dft.examples

/**
  * Created by jianyu on 3/26/17.
  */
import org.apache.spark.{HashPartitioner, Partitioner}
import org.apache.spark.{SparkConf, SparkContext}
import java.util.StringTokenizer

import scala.collection.mutable.{HashSet, ListBuffer}
import scala.io.StdIn


class ListHashPartitioner(partitions: Int,isHead: Boolean) extends Partitioner {
  def numPartitions: Int = partitions
  def head:Boolean=isHead

  def nonNegativeMod(x: Int, mod: Int): Int = {
    val rawMod = x % mod
    rawMod + (if (rawMod < 0) mod else 0)
  }

  //partition based only on the first entry
  def getPartition(key: Any): Int = key match {
    case null => 0
    case s:String =>
      val ss=s.split(" ")
      if(head){
        val s1=ss(0)
        nonNegativeMod(s1.hashCode, numPartitions)
      }else{
        val length=ss.length
        val s1=ss(length-1)
        nonNegativeMod(s1.hashCode, numPartitions)
      }
    //case _ => 0
  }

  override def equals(other: Any): Boolean = other match {
    case h: ListHashPartitioner =>
      h.numPartitions == numPartitions
    case _ =>
      false
  }

  override def hashCode: Int = numPartitions
}

object AdjancentList{
  def main(args:Array[String]){
    if (args.length == 0){
      System.err.println("Usage: AdjancentList <listMaxLength> <inputPath> <outputPath> <isPartitioned> [<numPartitions> <isHead>]")
      System.exit(1)
    }

    val maxLength = args(0).toInt
    val file = args(1)
    val outputPath = args(2)
    val isPartitioned = args(3).toBoolean
    val numPartitions = if(args.length == 5){
      args(4).toInt
    } else {
      16
    }
    val isHead = if(args.length==6){
      args(5).toBoolean
    }else{
      true
    }

    /**
    val file="/edges100.txt"
        val maxLength=5
        val outputPath="/adjOut"
        val isPartitioned=true
        val numPartitions=16
        val isHead=true
      */

    val conf = new SparkConf().setAppName("AdjancentList")
    val sc = new SparkContext(conf)

    val input = sc.textFile(file)

    val indexPartitioner = if(isPartitioned){
      new ListHashPartitioner(numPartitions,isHead)
    } else {
      new HashPartitioner(numPartitions)
    }

    //count the prefix of the max length from the raw input file
    val adjNode = input.flatMap(entry => {
      val edges = entry.split(",")
      val inEdge = edges(0)
      val outEdge = edges(1)
      val buffer = new ListBuffer[(String,(String,String))]()
      buffer += ((inEdge,("",outEdge)))
      buffer += ((outEdge,(inEdge,"")))
      buffer.toList
    })

    var adjList = adjNode.reduceByKey(indexPartitioner,(a,b) =>{
      /**(edges,(inEdges,outEdges))
        	edges/inEdges/outEdges are the edges separated by " ", e.g., "edge1 edge2 edge3"
        */
      val inEdgesA = a._1.split(" ")
      val inEdgesB = b._1.split(" ")
      val inEdgeSet = new HashSet[String]()
      for(edge <- inEdgesA) inEdgeSet += edge
      for(edge <- inEdgesB) inEdgeSet += edge
      inEdgeSet.remove("")

      val outEdgesA = a._2.split(" ")
      val outEdgesB = b._2.split(" ")
      val outEdgeSet = new HashSet[String]()
      for(edge <- outEdgesA) outEdgeSet += edge
      for(edge <- outEdgesB) outEdgeSet += edge
      outEdgeSet.remove("")

      val inBuilder = new StringBuilder()
      val outBuilder = new StringBuilder()
      inEdgeSet.addString(inBuilder," ")
      outEdgeSet.addString(outBuilder," ")

      (inBuilder.toString, outBuilder.toString)
    })
    // adjList.saveAsTextFile(outputPath)

    //extend the list until the max length is achieved.
    for{length <- 2 to maxLength}{
      //var length=2
      val adjList2 = adjList.flatMap(entry => {
        val list = entry._1
        val inEdges = entry._2._1
        val outEdges = entry._2._2

        //extend the list by adding the outEdges to the tail
        val result1 = for{outEdge <- outEdges.split(" "); if outEdge != "" } yield {
          (list + " " + outEdge, (inEdges, ""))
        }

        //exted the list by adding the inEdges to the head
        val result2 = for{inEdge <- inEdges.split(" "); if inEdge != "" } yield {
          (inEdge + " " + list, ("", outEdges))
        }

        result1 ++ result2
      })

      adjList = adjList2.reduceByKey(indexPartitioner,(a,b) => {

        val inEdgesA = a._1.split(" ")
        val inEdgesB = b._1.split(" ")
        val inEdgeSet = new HashSet[String]()
        for(edge <- inEdgesA) inEdgeSet += edge
        for(edge <- inEdgesB) inEdgeSet += edge
        inEdgeSet.remove("")
        inEdgeSet.remove(" ")

        val outEdgesA = a._2.split(" ")
        val outEdgesB = b._2.split(" ")
        val outEdgeSet = new HashSet[String]()
        for(edge <- outEdgesA) outEdgeSet += edge
        for(edge <- outEdgesB) outEdgeSet += edge
        outEdgeSet.remove("")
        outEdgeSet.remove(" ")

        val inBuilder = new StringBuilder()
        val outBuilder = new StringBuilder()
        inEdgeSet.addString(inBuilder," ")
        outEdgeSet.addString(outBuilder," ")

        (inBuilder.toString, outBuilder.toString)
      })//end of reduceByKey

    }//end of for

    println("count: " + adjList.count())
    adjList.collect().foreach(println)
    StdIn.readLine()
    sc.stop()
  }
}