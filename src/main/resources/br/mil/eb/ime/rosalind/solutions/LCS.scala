package br.mil.eb.ime.rosalind.solutions

import br.mil.eb.ime.rosalind.util.FASTAReader
import io.Source

/**
 * Created with IntelliJ IDEA.
 * User: jonasferreira
 * Date: 10/8/12
 * Time: 7:57 PM
 * To change this template use File | Settings | File Templates.
 */
object LCS extends App {

  val sequences = Source.fromFile("/home/jonas/Downloads/rosalind_lcs.txt").getLines.toList

  val minSequence = sequences.minBy(_.size)

  var keepGoing : Boolean = false
  var common : String = ""

  for (commonSize <- List.range(minSequence.size,0,-1)) {

    for (minCommon<-minSequence.sliding(commonSize).toList) {

      if (keepGoing) {
        println(common)
        keepGoing = false
        exit()
      } else {
        keepGoing = true
      }

      for (currentSequence<-sequences) {
        if (keepGoing && currentSequence.contains(minCommon)) {
          common = minCommon
        } else {
          keepGoing = false
          common = ""
        }
      }

    }
  }

}
