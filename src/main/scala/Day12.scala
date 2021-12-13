package com.ravikg

import scala.io.Source

object Day12 extends App {
  val input = Source.fromResource("day12.txt").getLines().toList

  val connections = input.map(c => c.split("-")).flatMap(c => Map(c(0) -> c(1), c(1) -> c(0)))
    .groupBy(_._1).view.mapValues(_.map(_._2)).toMap

  //Part 1
  def getPaths(conn: Map[String, List[String]], curNode: String, curPaths: List[List[String]]): List[List[String]] = {
    if (curNode.equals("end")) {
      curPaths.map(_ :+ curNode)
    } else if (curNode.equals("start") & curPaths.isEmpty) {
      val paths = List(List("start"))
      val nextNodes = conn.getOrElse(curNode, List[String]())
      val totalPaths = nextNodes.flatMap(node => getPaths(conn, node, paths))
      totalPaths
    }
    else {
      val paths = curPaths
        //filter empty List
        .filter(_.nonEmpty)
        //remove when cur node is small and already in the path
        .filter(path => !(curNode.equals(curNode.toLowerCase) & path.contains(curNode)))
        .map(_ :+ curNode)
      if (paths.nonEmpty) {
        val nextNodes = conn.getOrElse(curNode, List[String]()).filter(!_.equals("start"))
        val totalPaths = nextNodes.flatMap(node => getPaths(conn, node, paths)).filter(_.nonEmpty)
        totalPaths
      } else {
        List[List[String]]()
      }
    }
  }

  val p = getPaths(connections, "start", List[List[String]]())
  println(p.size)


  //Part 2
  def getPaths2(conn: Map[String, List[String]], curNode: String, curPaths: List[(Boolean, List[String])]): List[(Boolean, List[String])] = {
    if (curNode.equals("end")) {
      curPaths.map(path => (path._1, path._2 :+ curNode))
    } else if (curNode.equals("start") & curPaths.isEmpty) {
      val paths = List((false, List("start")))
      val nextNodes = conn.getOrElse(curNode, List[String]())
      val totalPaths = nextNodes.flatMap(node => getPaths2(conn, node, paths))
      totalPaths
    }
    else {
      val paths = curPaths
        //filter empty List
        .filter(_._2.nonEmpty)
        //remove when cur node is small and already in the path and one other small node is already present
        .filter(path => !(path._1 & curNode.equals(curNode.toLowerCase) & path._2.contains(curNode)))
        .map(path => (path._1 | (curNode.equals(curNode.toLowerCase) & path._2.contains(curNode)), path._2 :+ curNode))
      if (paths.nonEmpty) {
        val nextNodes = conn.getOrElse(curNode, List[String]()).filter(!_.equals("start"))
        val totalPaths = nextNodes.flatMap(node => getPaths2(conn, node, paths)).filter(_._2.nonEmpty)
        totalPaths
      } else {
        List[(Boolean, List[String])]()
      }
    }
  }

  val p2 = getPaths2(connections, "start", List[(Boolean, List[String])]())
  println(p2.size)
  
}
