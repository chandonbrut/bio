package br.mil.eb.ime.rosalind.util

import io.Source

/**
 * Created with IntelliJ IDEA.
 * User: jonas
 * Date: 10/9/12
 * Time: 2:30 PM
 * To change this template use File | Settings | File Templates.
 */
object SplicingReader {
  def readFromFile(path:String):List[DNA] = {
    val lines = Source.fromFile(path).getLines
    return {for (line <- lines) yield new DNA("unnamed",line.mkString)}.toList
  }

}
