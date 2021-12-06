package com.ravikg

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scalaz.Memo

// URL: https://adventofcode.com/2021/day/6
object Day6 extends App {

  val input = Source.fromResource("day6.txt")
    .getLines().toList.head.split(",").map(_.toInt)

// Part 1
  var loop = 0

  var outs = input.foldLeft((ListBuffer[Int]()))((output, fish) => {
    if(fish == 0) {
      output += 6
      output += 8
    } else {
      output += fish-1
    }

  })

  loop += 1
  while(loop < 80) {
    outs = outs.toList.foldLeft((ListBuffer[Int]()))((output, fish) => {
      if(fish == 0) {
        output += 6
        output += 8
      } else {
        output += fish-1
      }
    })
    loop = loop + 1
  }

  println(outs.size)

    //Part 2
  val ff = input.groupBy(identity).view.mapValues(_.size).toMap

  lazy val fishCount: ((Int, Int)) => BigInt =  Memo.mutableHashMapMemo[(Int, Int), BigInt] {
      case (_: Int, 0) => 1
      case (0, totalDays: Int) => fishCount(6, totalDays - 1) + fishCount(8, totalDays - 1)
      case (fishAge: Int, totalDays: Int) => fishCount(fishAge - 1, totalDays - 1)
  }

  println(ff.map((f,t) => t * fishCount(f, 256)).sum)

}