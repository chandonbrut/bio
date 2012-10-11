package br.mil.eb.ime.rosalind.solutions

import br.mil.eb.ime.rosalind.algo.RNATranslator

/**
 * Created with IntelliJ IDEA.
 * User: jonasferreira
 * Date: 10/8/12
 * Time: 7:57 PM
 * To change this template use File | Settings | File Templates.
 */
object PERM extends App {
  val i=6
  val intList = List.range(1,i+1).permutations.toList
  println(intList.size)
  for (l<-intList)
    println(l.mkString(" "))
}