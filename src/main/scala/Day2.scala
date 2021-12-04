package com.ravikg

import scala.io.Source

//URL: https://adventofcode.com/2021/day/2
object Day2 extends App {

  val moves = Source.fromResource("day2.txt").getLines().toList
  
  // Part 1
  val out = moves.foldRight((0, 0))((move, output) => {
    val s = move.split(" ")
    s(0) match {
      case "forward" => (output._1 + s(1).toInt, output._2)
      case "up" => (output._1, output._2 - s(1).toInt)
      case "down" => (output._1, output._2 + s(1).toInt)
    }
  })
  println(out._1 * out._2)

  // Part 2
  val out2 = moves.foldLeft((0, 0, 0))((output, move) => {
    val s = move.split(" ")
    s(0) match {
      case "forward" =>
        (output._1 + s(1).toInt,
        output._2  + output._3 * s(1).toInt,
        output._3)
      case "up" => (output._1, output._2, output._3 - s(1).toInt)
      case "down" => (output._1, output._2, output._3 + s(1).toInt)
    }
  })
  println(out2._1 * out2._2)

}
