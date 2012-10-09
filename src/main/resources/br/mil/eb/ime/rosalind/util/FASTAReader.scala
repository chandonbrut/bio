package br.mil.eb.ime.rosalind.util

import io.Source

/**
 * Created with IntelliJ IDEA.
 * User: jonas
 * Date: 10/9/12
 * Time: 2:30 PM
 * To change this template use File | Settings | File Templates.
 */
object FASTAReader {
  def readFromFile(path:String):List[DNA] = {
    val lines = Source.fromFile(path).mkString
    val m = lines.mkString.split(">")
    val cleanList = for (str <- m.toList.tail)
    yield str.replaceAll("[\r\n]"," ").replaceAll(" +"," ").replaceAll("A ","A").replaceAll("C ","C").replaceAll("G ","G").replaceAll("T ","T").split(" ")

    val superList = cleanList.flatten



    val dnas = for (dna <- superList.sliding(2,2))
    yield dna match { case List(a,b) => new DNA(a,b)}
    return dnas.toList
  }

}
