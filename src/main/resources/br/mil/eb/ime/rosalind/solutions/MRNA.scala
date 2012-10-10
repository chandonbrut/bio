package br.mil.eb.ime.rosalind.solutions

import br.mil.eb.ime.rosalind.algo.{RNATranslator, MotifFinder}

/**
 * Created with IntelliJ IDEA.
 * User: jonasferreira
 * Date: 10/8/12
 * Time: 7:57 PM
 * To change this template use File | Settings | File Templates.
 */
object MRNA extends App {
  val sequence="MMMAEIMVFSRLGRKSPIVLFFAGTYLQQFINLHYAHASPWSGGEDYV"
  var count:BigInt=3

  for (base <- sequence.toList) {
    println(base + " => " + count + " * " + RNATranslator.translateProtein(base.toString).size + " = " + count*RNATranslator.translateProtein(base.toString).size)
    count=count*RNATranslator.translateProtein(base.toString).size
  }
  println(count)
}