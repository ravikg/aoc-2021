package com.ravikg

import scala.io.Source

//URL https://adventofcode.com/2021/day/1
object Day1 extends App {
  val depths = Source.fromResource("day1.txt").getLines().toList.map(_.toInt)

  //Part 1
  val ans1 = depths.sliding(2).count(m => m(0) < m(1))
  println(ans1)

  //Part 2
  val ans2 = depths.sliding(3).map(_.sum).sliding(2).count(m => m(0) < m(1))
  println(ans2)
}
