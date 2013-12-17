package rosalind.solutions.coursera

import rosalind.util.{KMER, SetUtil}
import rosalind.algo.Graph

/**
 * Created by Jonas on 09/12/13.
 */
/*
Sample Input:
4

Sample Output:
0000110010111101
*/
object W5L3_UNIVERSAL_STRING_PROBLEM extends App {
  val k = 4
  println("Building arrangements...")
  val possible = SetUtil.binArrange(k)
  println("Building kmers...")
  val kmers = for (km<-possible) yield new KMER(km)
  println("Building de Bruijn...")
  val g = Graph.deBruijnAdj(kmers.toList)
  println("Getting eulerian cycle...")
  println(g.edges().distinct.size)
  val e = g.eulerianCycle()
  println(e.mkString("\n"))



  val labels = for (label<-e) yield label.label.content.split("")(1)
  println("Generating universal string...")
  println(labels.mkString(""))

}
