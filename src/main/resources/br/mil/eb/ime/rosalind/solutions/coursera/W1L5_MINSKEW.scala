package br.mil.eb.ime.rosalind.solutions.coursera

import br.mil.eb.ime.rosalind.util.DNA

/**
 * Created with IntelliJ IDEA.
 * User: jonas
 * Date: 11/13/13
 * Time: 10:05 PM
 * To change this template use File | Settings | File Templates.
 */
object W1L5_MINSKEW extends App {

  val sequence = io.Source.fromFile("/home/jonas/Downloads/dataset_7_6.txt").mkString("")

  val indexes = for (i<-List.range(0,sequence.size+1))
    yield (new DNA("unnamed",sequence.substring(0,i))).gminusc()

  val min = indexes.min
  val withIndex = indexes.zipWithIndex

  val results = for ( t <- withIndex.filter(_._1 == min)) yield t._2


  println(results.mkString(" "))


}
