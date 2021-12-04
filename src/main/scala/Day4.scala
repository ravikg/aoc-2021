package com.ravikg

import scala.collection.mutable.ListBuffer
import scala.io.Source
import util.control.Breaks._

// URL: https://adventofcode.com/2021/day/4
object Day4 extends App {
  val input = Source.fromResource("day4.txt").getLines() //iterator
  val draws = input.next().split(",").map(_.toInt)

  var boards = ListBuffer[Array[Array[Int]]]()

  //populate boards
  while(input.hasNext) {
    //escape empty line
    input.next()

    val board = Array.ofDim[Int](5, 5)
    for (i <- 0 to 4) {
      val r = input.next()
      val row = r.trim.split("\\s+").map(_.toInt)
      board(i) = row
    }
    boards += board
  }


//  println(boards(0)(0))

  // given a board and a draw; update the board and return its location
  // assumes single match; for multiple match call the same method
  // if draw was present in board
  // update that location with value = -1 and returns its location
  // else returns -1, -1
  def updateDraw(board: Array[Array[Int]], draw: Int): (Int, Int) = {
    for (i <- 0 to 4) {
      for (j <- 0 to 4) {
        if (board(i)(j) == draw) {
          board(i)(j) = -1
          return (i, j)
        }
      }
    }
    return (-1, -1)
  }

  //given a board and a locaiton (i,j) check if row i or col j has matched all draw
  // i.e. each cell in row/col == -1
  def checkBoard(board: Array[Array[Int]], loc:(Int, Int)): Boolean = {
    var row = true
    for(j <- 0 to 4) {
      if(board(loc._1)(j) != -1) row = false
    }

    var col = true
    for(i <- 0 to 4) {
      if(board(i)(loc._2) != -1) col = false
    }
    row | col
  }

  // Part 1 uncomment to run this
  // 1st problem
  //draw one at a time
  /*draws.foreach(draw => {
    //check each board
    boards.foreach(board => {
      val loc = updateDraw(board, draw)
      if(loc != (-1,-1)) {
        val check = checkBoard(board, loc)
        if (check) {
          board.foreach(row => println(row.mkString(" ")))
          //find sum of this board
          var sum = 0
          for (i <- 0 to 4) {
            for (j <- 0 to 4) {
              if (board(i)(j) != -1) sum += board(i)(j)
            }
          }
          println(sum)
          println(sum * draw)
          break; // break at the 1st win
        }
      }
    })
  })*/

  // Part 2
  draws.foreach( draw => {
    var removeBoard = ListBuffer[Int]()
    for(bi <- 0 until boards.size) {
      val board = boards(bi)

      val loc = updateDraw(board, draw)
      if(loc != (-1,-1)) {
        val check = checkBoard(board, loc)
        if (check) removeBoard += bi
      }
    }
    //remove winning boards
//    println(boards.size)
//    println(removeBoard)

    //stop if only 1 board remains and it is winning
    if(boards.size == 1 & removeBoard.size ==1) {
      val board = boards.head
//      board.foreach(row => println(row.mkString(" ")))
      //find sum of this board
      var sum = 0
      for (i <- 0 to 4) {
        for (j <- 0 to 4) {
          if (board(i)(j) != -1) sum += board(i)(j)
        }
      }
//      println(sum)
      println(sum * draw)
      break; // break at the 1st win
    }

    if(boards.size != 1) boards = boards.zipWithIndex.filter( p => !(removeBoard contains p._2) ).map( _._1 )
  })

}
