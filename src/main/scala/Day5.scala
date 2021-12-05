package com.ravikg

import scala.collection.mutable.ListBuffer
import scala.io.Source
import util.control.Breaks._

// URL: https://adventofcode.com/2021/day/5
object Day5 extends App {
  val input = Source.fromResource("day5.txt").getLines() //iterator
  val lines = input.map(_.split(" -> ").map(_.split(",").map(_.toInt).toList).toList)
    //.filter(line => line(0)(0) == line(1)(0) | line(0)(1) == line(1)(1))
    .toList

  //lines.foreach(println)

  var counts = scala.collection.mutable.Map[(Int, Int), Int]().withDefaultValue(0)

  lines.foreach(line => {
      //horizontal line
      if(line(0)(1) == line(1)(1)) {
          if(line(0)(0) < line(1)(0)) {
            for(i <- line(0)(0) to line(1)(0)) {
                counts((i, line(0)(1))) += 1
            }
          }
          else {
            for(i <- line(1)(0) to line(0)(0)) {
                counts((i, line(0)(1))) += 1
            }
          }
      }

      //vertical line
      if(line(0)(0) == line(1)(0)) {
          if(line(0)(1) < line(1)(1)) {
            for(j <- line(0)(1) to line(1)(1)) {
                counts((line(0)(0), j)) += 1
            }
          }
          else {
            for(j <- line(1)(1) to line(0)(1)) {
              counts((line(0)(0), j)) += 1
            }
          }
      }

      //diagonal line
      if(line(0)(0) != line(1)(0) & line(0)(1) != line(1)(1)) {
        //   println(line)
          val l = (line(0)(0) - line(1)(0)).abs
          if(line(0)(0) < line(1)(0)) {
              if(line(0)(1) < line(1)(1)) {
                for(d <- 0 to l) {
                    counts((line(0)(0)+d, line(0)(1)+d)) += 1
                }
              } else {
                  for(d <- 0 to l) {
                    counts((line(0)(0)+d, line(0)(1)-d)) += 1
                }
              }
          } else {
            if(line(0)(1) < line(1)(1)) {
                for(d <- 0 to l) {
                    counts((line(0)(0)-d, line(0)(1)+d)) += 1
                }
              } else {
                  for(d <- 0 to l) {
                    counts((line(0)(0)-d, line(0)(1)-d)) += 1
                }
              }
          }
          
      }

  })
//   counts.foreach(println)
  println(counts.count(_._2 > 1))

}