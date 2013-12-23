package rosalind.solutions.coursera

import scala.collection.mutable.ArrayBuffer
import rosalind.algo.{CityGraph, Graph}

/**
 * Created by Jonas on 23/12/13.
 */
/*
Sample Input:
     4
     4
     1 0 2 4 3
     4 6 5 2 1
     4 4 5 2 1
     5 6 8 5 3
     -
     3 2 4 0
     3 2 4 2
     0 7 3 3
     3 3 0 2
     1 3 2 2

Sample Output:
     34
 */

object W6L2_MANHATTAN_TOURIST extends App {
  val n = 10
  val m = 16

  val down = Array.ofDim[Int](n,m+1)
  val right = Array.ofDim[Int](n+1,m)

  val downMatrixString = "4 1 3 3 4 1 1 2 4 1 1 4 4 3 3 1 3\n3 0 2 0 2 4 0 3 4 2 0 4 1 0 4 1 4\n2 4 2 0 4 0 3 1 1 0 0 2 1 1 4 0 2\n4 3 3 3 4 4 4 3 0 4 2 0 2 4 3 0 0\n1 2 4 3 0 4 1 1 4 1 1 3 1 2 4 4 1\n2 4 3 0 3 1 2 0 4 3 3 3 4 3 2 2 3\n1 1 0 4 1 4 0 1 1 3 4 2 1 1 1 0 0\n1 2 1 1 2 3 2 0 0 2 2 2 3 3 3 2 0\n3 1 2 0 1 3 0 1 3 0 4 0 0 0 2 1 1\n1 4 4 2 3 2 3 0 4 0 1 0 1 2 1 0 4"
  val rightMatrixString = "2 3 0 0 2 1 4 3 4 1 3 2 1 3 2 0\n2 3 2 1 3 0 0 1 2 2 0 3 1 4 2 4\n0 4 2 3 3 1 4 2 0 2 4 3 4 2 0 4\n3 3 2 2 2 1 2 3 0 0 4 1 3 0 4 0\n0 1 2 1 1 0 2 2 3 2 3 1 0 3 3 1\n2 0 4 4 1 4 2 4 4 2 1 4 2 4 0 3\n0 3 2 3 4 0 4 3 0 1 2 0 3 2 2 1\n0 1 1 1 3 4 2 4 1 3 1 1 2 0 4 3\n1 0 0 0 2 2 4 0 3 1 1 1 3 0 4 0\n1 4 0 3 2 2 0 1 2 3 4 1 1 4 4 4\n4 4 3 4 1 2 2 4 4 4 0 3 2 1 2 0"


  var i=0;
  var j=0;
  for (d <- downMatrixString.split("\n")) {
    for (x <- d.split(" ")){
      down(i).update(j,x.toInt)
      j = j + 1
    }
    j = 0
    i = i+1
  }

  i=0;
  j=0;
  for (d <- rightMatrixString.split("\n")) {
    for (x <- d.split(" ")){
      right(i).update(j,x.toInt)
      j = j + 1
    }
    j = 0
    i = i+1
  }

  val city = new CityGraph(n,m,down,right)
  city.print()
  println("maxW = " + city.maxWeight)

//  for (k <- List.range(0,down.length)) {
//    println(down(k).mkString(" "))
//  }
//
//  println("-")
//
//  for (k <- List.range(0,right.length)) {
//    println(right(k).mkString(" "))
//  }


}
