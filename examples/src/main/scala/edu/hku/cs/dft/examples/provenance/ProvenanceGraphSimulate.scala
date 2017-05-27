import org.apache.spark.{SparkConf, SparkContext}
/**
  * Created by jianyu on 5/11/17.
  */
object Connected {
  def main(args: Array[String]) {
//    val conf = new SparkConf()

    println("Program started")
//    conf.setMaster("local[2]")

    val file = "edges.txt"

    val partitions = 16

    val iteration = 8

    val text = sc.textFile(file)
    println(text)
    var edges = text.map(t => {
      val s_arr = t.split(",")//\\s+")
//      println(s_arr)
      (s_arr(0), s_arr(1))
    })
    println("here")
    val nodes = edges.flatMap(edge => Array(edge._1, edge._2)).distinct()

    var label_node = nodes.zipWithIndex().map{
      case(node, id) => (node, (id, 0, Array(node, -1)))
    }

    for (i <- 1 to iteration) {
      // group nodes with the same id
      val new_label = label_node.join(edges).map(t => (t._2._2, (t._2._1._1, i, t._2._1._3)))
      //new_label.foreach(println)
        
      val m_label = new_label.union(label_node)
      label_node = m_label.reduceByKey((x, y) => {
        if (x._1 != y._1) {
          if (x._1 < y._1)
            (x._1, x._2, (x._3 ++ y._3).distinct )
          else
            (y._1, y._2, (y._3 ++ x._3).distinct )
        } else {
          if (x._2 < y._2)
            (x._1, x._2, (x._3 ++ y._3).distinct )
          else
            (y._1, y._2, (y._3 ++ x._3).distinct )
        }
      }, numPartitions = partitions)
    }

    label_node.collect()
    label_node.foreach( x =>
      if(x._1 == "060339") 
        println(x._1+" "+x._2._3.size + " ")
      ) //+ x._2._3.mkString(",")))
//    lc.setCaptureLineage(false)

//    val lineage = label_node.getLineage()

//    lineage.filter(_ == 19L)
//      .goBackAll()
//      .count()

    println("Program End")
    sc.stop()
  }
}
