package com.ravikg

import scala.io.Source
import scala.language.postfixOps

object Day11 extends App {

  val input = Source.fromResource("day11.txt").getLines().toList

  val rows = input.map(_.split("").map(_.toInt).toList zipWithIndex)
  var matrix = rows.zipWithIndex.flatMap({ case (l, i) => l.map(p => (p._2, i, p._1)) })
  val grid = collection.mutable.Map() ++ matrix.map(a=> (a._1, a._2) -> (a._3, true)).toMap

    // Part 1
    var score:BigInt = 0
    for (i <- 0 until 100) {
        score += nextStep(grid)
    }

    println(score)

    // Part 2
    val grid2 = collection.mutable.Map() ++ matrix.map(a=> (a._1, a._2) -> (a._3, true)).toMap
    var steps = 0
    score = 0
    while(score != 100) {
        steps += 1
        score = nextStep(grid2)
    }
    println(steps)

    def nextStep(grid:collection.mutable.Map[(Int, Int), (Int, Boolean)]) : Int = {
        var numGlows = 0;
        for (i <- 0 until 10) {
            for (j <- 0 until 10) {
                var energy = grid((i, j))
                if(energy._1 == 9) {
                    energy = (0, false)
                    grid((i, j)) = energy
                    glows(grid, i, j)
                } else if(energy._2) {
                    grid((i, j)) = (energy._1 + 1, energy._2)
                }
            }
        }

        for (i <- 0 until 10) {
            for (j <- 0 until  10) {
                var energy = grid((i, j))
                if(energy._1 == 0) {
                    numGlows +=1
                    energy = (0, true)
                    grid((i, j)) = energy
                }
            }
        }
        numGlows
    }

    // abc
    // p*r
    // xyz
    def glows(g:collection.mutable.Map[(Int, Int), (Int, Boolean)], i: Int, j: Int) = {

        update(g, i-1, j-1)
        update(g, i, j-1)
        update(g, i+1, j-1)

        update(g, i-1, j)
        update(g, i+1, j)

        update(g, i-1, j+1)
        update(g, i, j+1)
        update(g, i+1, j+1)

    }


    def update(g:collection.mutable.Map[(Int, Int), (Int, Boolean)], i: Int, j: Int): Boolean = {
        val a = g.getOrElse((i, j), (-1, false)) //else cond. is for out of the sides
        var nexglow = false
        if(a._2) {
            if(a._1 == 9) {
                g((i, j)) = (0, false)
                nexglow = true
                glows(g, i, j)
            }
            else {
                g((i, j)) = (a._1 + 1, a._2)
            }
        }
        nexglow
    }

}
