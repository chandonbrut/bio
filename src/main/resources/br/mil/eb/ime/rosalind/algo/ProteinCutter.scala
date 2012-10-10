package br.mil.eb.ime.rosalind.algo

/**
 * Created with IntelliJ IDEA.
 * User: jonas
 * Date: 10/10/12
 * Time: 10:37 AM
 * To change this template use File | Settings | File Templates.
 */
object ProteinCutter {
  def cut(proteinSequence : String) : List[String] = {
    val lowerStart = proteinSequence.indexOf("M")
    val upperStart = proteinSequence.lastIndexOf("M")
    val lowerStop = proteinSequence.indexOf("$")
    val upperStop = proteinSequence.lastIndexOf("$")

    if (lowerStart<0 || upperStop <0 || lowerStart>upperStop)
      return Nil

    val notJunk = proteinSequence.substring(lowerStart,upperStop)
    val toRead = notJunk.split("\\$").toList
    val subjects = {
      for (s<-toRead if (!s.equals("")))
         yield s
      }.toList

    for (s<-subjects; i<-List.range(0,s.size) if s.substring(i).startsWith("M"))
      yield s.substring(i)
  }

}