package br.mil.eb.ime.rosalind.algo

/**
 * Created with IntelliJ IDEA.
 * User: jonasferreira
 * Date: 10/8/12
 * Time: 6:59 PM
 * To change this template use File | Settings | File Templates.
 */
object Hamming {

  def calculate(s1:String,s2:String) : Int = {
    val zipped = s1 zip s2
    var counter=0
    zipped.foreach(_ match {
      case (a,b) if a==b =>
      case _ => counter=counter+1
    })

    return counter
  }
}
