package com.ravikg

import java.lang.Math
import scala.io.Source

// URL: https://adventofcode.com/2021/day/7
object Day7 extends App {
//  val input = List(16,1,2,0,4,2,7,1,2,14)
  val input = Source.fromResource("day7.txt")
    .getLines().toList.head.split(",").map(_.toInt)
  val min = input.min
  val max = input.max
  
  // Part 1
  var minFuel = Integer.MAX_VALUE;
  for (i <- min to max) {
    val mf= input.foldLeft(0)((out, in) => {
      Math.abs(i - in) + out
    })
    if(mf < minFuel) minFuel = mf
  }

  println(minFuel)

  // Part 2
  var minFuel2: BigInt = Integer.MAX_VALUE;
  for (i <- min to max) {
    val mf= input.foldLeft(0:BigInt)((out, in) => {

      val n = Math.abs(i - in)
      ((n * (n+1))/2) + out
    })
    if(mf < minFuel2) minFuel2 = mf
  }

  println(minFuel2)

}
