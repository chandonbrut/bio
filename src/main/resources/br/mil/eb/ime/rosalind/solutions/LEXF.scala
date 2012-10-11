package br.mil.eb.ime.rosalind.solutions

import br.mil.eb.ime.rosalind.algo.RNATranslator

/**
 * Created with IntelliJ IDEA.
 * User: jonasferreira
 * Date: 10/8/12
 * Time: 7:57 PM
 * To change this template use File | Settings | File Templates.
 */
object LEXF extends App {
  val sequence="P G R H"
  var count=2

  def combine(l: List[String], k:Int): List[String] = {
    if (k==2) {
      val retorno = for (i<-l; j<-l)
        yield List(i,j).mkString
      return retorno.toList
    } else {
      val retorno = for (i<-l; j<-combine(l,k-1))
        yield List(i,j).mkString
      return retorno.toList
    }
  }

  val chars = sequence.split(" ").toList
  val possible = combine(chars,4)
  val words = {
    for (word <- possible)
      yield word.mkString
  }.toList

  words.foreach(println)

}