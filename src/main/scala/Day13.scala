package com.ravikg

import scala.io.Source

// URL: https://adventofcode.com/2021/day/13
object Day13 extends App {

  val input = Source.fromResource("day13.txt").getLines()
    .map(_.split(",").map(_.toInt))
    .map(a => (a(0), a(1)))
    .toList
    .to(Set)

  def foldY(input: Set[(Int, Int)], y: Int): Set[(Int, Int)] = {
    val below = input.filter(_._2 > y)
    val newUp = below.map(a => (a._1, 2 * y - a._2)) // Ybelow-Y = Y-Yup => Yu = 2*Y - Yb
    input.filter(_._2 < y) ++ newUp
  }

  def foldX(input: Set[(Int, Int)], x: Int): Set[(Int, Int)] = {
    val below = input.filter(_._1 > x)
    val newUp = below.map(a => (2 * x - a._1, a._2)) // Xright-X = X-Xleft => Xl = 2*X - Xr
    input.filter(_._1 < x) ++ newUp
  }

  /*
  fold along x=655
  fold along y=447
  fold along x=327
  fold along y=223
  fold along x=163
  fold along y=111
  fold along x=81
  fold along y=55
  fold along x=40
  fold along y=27
  fold along y=13
  fold along y=6
  */
  
  //Part 1
  val X = 655
  println(foldX(input, X).size)
  
  //Part 2
  var out = input
  out = foldX(out, 655)
  out = foldY(out, 447)
  out = foldX(out, 327)
  out = foldY(out, 223)
  out = foldX(out, 163)
  out = foldY(out, 111)
  out = foldX(out, 81)
  out = foldY(out, 55)
  out = foldX(out, 40)
  out = foldY(out, 27)
  out = foldY(out, 13)
  out = foldY(out, 6)

  for (j <- 0 to out.map(_._2).max) {
    for (i <- 0 to out.map(_._1).max) {
      if (out.contains((i, j))) print(" # ")
      else print(" . ")
    }
    println("")
  }
}
