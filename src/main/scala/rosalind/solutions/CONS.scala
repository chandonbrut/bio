package br.mil.eb.ime.rosalind.solutions

import br.mil.eb.ime.rosalind.algo.{DNAProfiler, MotifFinder}
import io.Source

/**
 * Created with IntelliJ IDEA.
 * User: jonasferreira
 * Date: 10/8/12
 * Time: 7:57 PM
 * To change this template use File | Settings | File Templates.
 */
object CONS extends App {

  val src = Source.fromFile("/home/jonas/Downloads/rosalind_cons.txt")


  val lines = src.getLines

  val sequences = for (line <- lines ) yield line.mkString

  val profile = DNAProfiler.profile(DNAProfiler.transposeStrings(sequences.toList))


//  println(DNAProfiler.consensus(List(profile))

  var aC:String = ""
  var cC:String = ""
  var gC:String = ""
  var tC:String = ""

  for (j<-profile)
    j match {
      case (a,c,g,t) => {
        aC=aC+a
        cC=cC+c
        gC=gC+g
        tC=tC+t
      }
    }
  println("A: " + aC.sliding(1,1).mkString(" "))
  println("C: " + cC.sliding(1,1).mkString(" "))
  println("G: " + gC.sliding(1,1).mkString(" "))
  println("T: " + tC.sliding(1,1).mkString(" "))
}
