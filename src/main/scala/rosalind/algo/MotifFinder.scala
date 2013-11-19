package br.mil.eb.ime.rosalind.algo

/**
 * Created with IntelliJ IDEA.
 * User: jonasferreira
 * Date: 10/8/12
 * Time: 7:38 PM
 * To change this template use File | Settings | File Templates.
 */
object MotifFinder {
  def findMotif(motif:String,sequence:String) : List[Int] = {
    val splitted:List[String] = sequence.sliding(motif.length).toList
    val indexes:List[Int] = for ( i <- List.range(0,splitted.size)
                                  if splitted(i).equals(motif) )
    yield i+1
    return indexes
  }

}
