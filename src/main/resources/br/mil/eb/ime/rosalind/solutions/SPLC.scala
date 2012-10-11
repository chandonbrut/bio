package br.mil.eb.ime.rosalind.solutions

import br.mil.eb.ime.rosalind.util.{DNA, SplicingReader, FASTAReader}

/**
 * Created with IntelliJ IDEA.
 * User: jonasferreira
 * Date: 10/8/12
 * Time: 7:57 PM
 * To change this template use File | Settings | File Templates.
 */
object SPLC extends App {

  val dnas = SplicingReader.readFromFile("/home/jonas/Downloads/rosalind_splc.txt")

  val introns = dnas.tail
  val strand = dnas.head
  var fullString = strand.sequence

  for (intron<-introns)
    fullString = fullString.split(intron.sequence).mkString

  println(new DNA("unnamed",fullString).codeProtein)
}
