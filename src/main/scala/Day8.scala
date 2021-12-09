package com.ravikg

import scala.io.Source

// URL: https://adventofcode.com/2021/day/8
object Day8 extends App {

  val input = Source.fromResource("day8.txt")
    .getLines().toList
  val len = List(2, 4, 3, 7) // length of 1, 4, 7, 8
  // Part 1
  val outs = input.map(_.split('|').last)
    .flatMap(_.split(" ").toList)
    .count(o => len.contains(o.length))
  println(outs)

  //Part 2
  val res = input.map(line1 => {
    val line = line1.split('|')
    val ins = line(0).trim.split(" ").toList
    val outp = line(1).trim.split(" ").toList
    findNum(ins, outp)
  }).sum

  println(res)

  def findNum(ins: List[String], outp: List[String]): Int = {
    val words = ins.map(_.split("").sorted.mkString)
    //no - length
    // 0,6,9-6
    // 1-2;
    // 2,3,5-5;
    // 4-4;
    // 7-3;
    // 8-7
    val one = words.filter(_.length == 2).head
    val four = words.filter(_.length == 4).head
    val seven = words.filter(_.length == 3).head
    val eight = words.filter(_.length == 7).head

    val six = words.filter(_.length == 6).filter(m => (one diff m).length != 0).head
    val nine = words.filter(_.length == 6).filter(m => (four diff m).length == 0).head
    val zero = words.filter(_.length == 6).filter(m => (six diff m).length != 0).filter(m => (nine diff m).length != 0).head

    val three = words.filter(_.length == 5).filter(m => (one diff m).length == 0).head
    val five = words.filter(_.length == 5).filter(m => (m diff six).length == 0).head
    val two = words.filter(_.length == 5).filter(m => (three diff m).length != 0).filter(m => (five diff m).length != 0).head

    val map = Map(one.toString -> 1, two -> 2, three -> 3, four -> 4, five -> 5, six -> 6, seven -> 7, eight -> 8, nine -> 9, zero -> 0)

    outp.map(_.split("").sorted.mkString).map(m => map.getOrElse(m, -1)).foldLeft(0)((out, in) => out * 10 + in)
  }
  
}
