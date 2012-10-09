package br.mil.eb.ime.rosalind.solutions

import br.mil.eb.ime.rosalind.algo.DNAProfiler
import io.Source
import br.mil.eb.ime.rosalind.util.{FASTAReader, DNA}
import collection.parallel.mutable

/**
 * Created with IntelliJ IDEA.
 * User: jonasferreira
 * Date: 10/8/12
 * Time: 7:57 PM
 * To change this template use File | Settings | File Templates.
 */
object GRPH extends App {

  val dnas = FASTAReader.readFromFile("/home/jonas/Downloads/rosalind_grph.txt")

  val toGoList = {
    for (dna1 <- dnas; dna2<-dnas)
    yield (dna1,dna2)
  }.toList

  var comput = List[(String,String)]()

  for (toCompare <- toGoList)
    toCompare match {
      case (a,b) if (a.fits(b,3)
        && !a.name.equals(b.name)
        && !comput.contains((a.name,b.name))
        && !comput.contains((b.name,a.name))
      ) => {
        println(a.name + " " + b.name)
        comput = comput ::: List((a.name,b.name),(b.name,a.name))
      }
      case _ =>
    }

//  println(reversedList)


}
