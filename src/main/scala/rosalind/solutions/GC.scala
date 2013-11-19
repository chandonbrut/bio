package br.mil.eb.ime.rosalind.solutions

import br.mil.eb.ime.rosalind.util.{FASTAReader, DNA}
import io.Source

/**
 * Created with IntelliJ IDEA.
 * User: jonas
 * Date: 10/9/12
 * Time: 1:01 PM
 * To change this template use File | Settings | File Templates.
 */
object GC extends App {

  val dnas = FASTAReader.readFromFile("/home/jonas/Downloads/rosalind_gc.txt")

  val maxDna = dnas.toList.maxBy(_.gcContent)

  println(maxDna.name)
  println(maxDna.gcContent.formatted("%2.2f")+"%")



}
