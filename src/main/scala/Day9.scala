package com.ravikg

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.language.postfixOps
import scala.math.Ordered.orderingToOrdered

// URL: https://adventofcode.com/2021/day/9
object Day9 extends App {

  val input = Source.fromResource("day9.txt").getLines().toList

  val matrix = input.map(_.split("").map(_.toInt).toList zipWithIndex)
  var output = matrix.zipWithIndex.flatMap({ case (l, i) => l.map(p => (i, p._2, p._1)) })
  val m = output.map(a=> (a._1, a._2) -> a._3).toMap

  val Y = matrix.size
  val X = matrix.head.size
  val max = 10 //as max in input is 9

  var countLowPoints = 0
  var lowPointsList = ListBuffer[(Int, Int)]()
  for(j <- 0 until X) {
    for (i <- 0 until Y) {
      val point = m.getOrElse((i, j), max)
      if (m.getOrElse((i - 1, j), max) > point)
        if (m.getOrElse((i + 1, j), max) > point)
          if (m.getOrElse((i, j - 1), max) > point)
            if (m.getOrElse((i, j + 1), max) > point) {
              countLowPoints += 1 + point
              lowPointsList += ((i, j))
            }
    }
  }

  println(countLowPoints)

  // Part 2
  val basinSizeList = ListBuffer[Int]()

  val h = lowPointsList.head
  val d  = basinSize(h._1, h._2, ListBuffer[(Int, Int)]())
  val bs: List[Int] = lowPointsList.map((i, j) => basinSize(i, j, ListBuffer[(Int, Int)]())).toList.sortWith(_ > _)
  println(bs(0)*bs(1)*bs(2))

  def basinSize(i: Int, j:Int, points: ListBuffer[(Int, Int)]): Int = {
    points += ((i, j))
      val up = if (m.getOrElse((i, j - 1), max) < 9 & !points.contains((i, j-1))) basinSize(i, j - 1, points) else 0
      val down = if (m.getOrElse((i, j + 1), max) < 9 & !points.contains((i, j+1))) basinSize(i, j + 1, points) else 0
      val left = if (m.getOrElse((i - 1, j), max) < 9 & !points.contains((i-1, j))) basinSize(i - 1, j, points) else 0
      val right = if (m.getOrElse((i + 1, j), max) < 9 & !points.contains((i+1, j))) basinSize(i + 1, j, points) else 0
      1 + up + down + left + right

  }


}
