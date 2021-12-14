package com.ravikg

import scalaz.Memo
import scalaz.Scalaz._

import scala.io.Source

// URL: https://adventofcode.com/2021/day/14
object Day14 extends App {

  val input = Source.fromResource("day14.txt").getLines()
    .map(_.split(" -> ")).map(a => a(0) -> a(1)).toMap

  def poly(template: String, map: Map[String, String]): String = {
    template.sliding(2).foldLeft("")((out, a) => {
      out + a(0) + map(a)
    }) + template.last
  }

  var temp = "PKHOVVOSCNVHHCVVCBOH"
  //Part 1
  for (i <- 0 until 10) {
    temp = poly(temp, input)
  }
  val part1 = temp.groupBy(identity).mapValues(_.size).map(_._2)

  println(part1.max - part1.min)

  //Part 2

  lazy val polyCount: ((String, Int)) => Map[Char, BigInt] = Memo.mutableHashMapMemo {
    case (str: String, 1) => Map(str(0) -> BigInt(1)) |+| Map(input(str).head -> BigInt(1))
    case (str: String, step: Int) => polyCount(str(0) + input(str), step - 1) |+| polyCount(input(str) + str(1), step - 1)
  }

  var part2 = temp.sliding(2).map(n => polyCount(n, 40)).foldLeft(Map('H' -> BigInt(1)))((out, a) => out |+| a).map(_._2)

  println(part2.max - part2.min)

}
