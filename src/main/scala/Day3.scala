package com.ravikg

import scala.io.Source
import scala.math.Ordered.orderingToOrdered
import scala.util.control.Breaks.break

// URL : https://adventofcode.com/2021/day/3
object Day3 extends App {

  // Part 1

  val bits = Source.fromResource("day3.txt").getLines()
    .map(_.split("").toList)
    .map(l => (l.indices zip l).toMap)
    .toList
    .flatten
    .groupBy(_._1)
    .mapValues(_.map(_._2).groupBy(identity).mapValues(_.size))
    .toList.sortBy(_._1)

  val out = bits.foldLeft(("",""))((output, bit) => {
    if(bit._2.get("0") > bit._2.get("1")) (output._1+"0", output._2+"1")
    else (output._1+"1", output._2+"0")
  })

  println(Integer.parseInt(out._1, 2) * Integer.parseInt(out._2, 2))

  // Part 2
  def top(bits: List[String], ind: Int, maxMin: String): List[String] = {

    val indBit = bits
      .map(_.charAt(ind))
      .groupBy(identity)
      .mapValues(_.size)

    maxMin match {
      case "max" => {
        if(indBit.get('1') >= indBit.get('0'))
          bits.filter(_.charAt(ind) == '1')
        else
          bits.filter(_.charAt(ind) == '0')
      }
      case "min" => {
        if(indBit.get('0') <= indBit.get('1'))
          bits.filter(_.charAt(ind) == '0')
        else
          bits.filter(_.charAt(ind) == '1')
      }
    }
  }

  val bits2 = Source.fromResource("day3.txt").getLines().toList
  var b = bits2;
  var i = 0;
  while(b.size > 1) {
    b = top(b, i, "max")
    i = i + 1;
  }
  val maxBit = b

  b = bits2;
  i = 0;
  while(b.size > 1) {
    b = top(b, i, "min")
    i = i + 1;
  }

  val minBit = b

  println(Integer.parseInt(maxBit.head, 2) * Integer.parseInt(minBit.head, 2))


}
