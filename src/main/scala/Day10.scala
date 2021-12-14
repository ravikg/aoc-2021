package com.ravikg

import scala.collection.mutable.Stack
import scala.io.Source

// URL: https://adventofcode.com/2021/day/10
object Day10 extends App {
  val input = Source.fromResource("day10.txt").getLines().toList

  def isLegalScore(line: String): Int = {

    val chars = line.toCharArray
    var out = true
    var ints = Stack[Char]()
    var score = 0
    for(i <- 0 until chars.length) {
      val c = chars(i)
      if(score == 0) {
        c match {
          case ')' => if (ints.pop() != '(') score = 3
          case ']' => if (ints.pop() != '[') score = 57
          case '}' => if (ints.pop() != '{') score = 1197
          case '>' => if (ints.pop() != '<') score = 25137
          case a: Char => ints.push(a)
        }
      }
    }
    score
  }

  println(input.map(isLegalScore).sum)

  val incomplete = input.filter(line => isLegalScore(line) == 0)

  def part2(line: String): BigInt = {

    val chars = line.toCharArray
    var out = true
    var ints = Stack[Char]()
    var score = 0
    for(i <- 0 until chars.length) {
      val c = chars(i)
      if(score == 0) {
        c match {
          case ')' => if (ints.pop() != '(') score = 3
          case ']' => if (ints.pop() != '[') score =57
          case '}' => if (ints.pop() != '{') score =1197
          case '>' => if (ints.pop() != '<') score =25137
          case a: Char => ints.push(a)
        }
      }
    }
    var score2: BigInt = 0
    while (ints.nonEmpty) {
      score2 = ints.pop() match {
        case '(' => score2 * 5 + 1
        case '[' => score2 * 5 + 2
        case '{' => score2 * 5 + 3
        case '<' => score2 * 5 + 4
      }
    }
    score2
  }
  val o = incomplete.map(part2).sorted
  
  print(o((o.length-1) / 2))


}
