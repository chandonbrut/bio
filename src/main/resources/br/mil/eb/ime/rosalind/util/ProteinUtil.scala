package br.mil.eb.ime.rosalind.util

/**
 * Created with IntelliJ IDEA.
 * User: jonas
 * Date: 10/10/12
 * Time: 10:37 AM
 * To change this template use File | Settings | File Templates.
 */
object ProteinUtil {
  def cut(proteinSequence : String) : List[String] = {
    val lowerStart = proteinSequence.indexOf("M")
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

  def intWeight(protein : String) : Int = {
    val weight = {
      for (amino <- protein.toCharArray)
      yield amino.toString match {
        case "G" => 57
        case "A" => 71
        case "S" => 87
        case "P" => 97
        case "V" => 99
        case "T" => 101
        case "C" => 103
        case "I" => 113
        case "L" => 113
        case "N" => 114
        case "D" => 115
        case "K" => 128
        case "Q" => 128
        case "E" => 129
        case "M" => 131
        case "H" => 137
        case "F" => 147
        case "R" => 156
        case "Y" => 163
        case "W" => 186
      }
    }.toList.sum
    return weight
  }
}