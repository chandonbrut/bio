package br.mil.eb.ime.rosalind.algo
import scala.collection.mutable
/**
 * Created with IntelliJ IDEA.
 * User: jonas
 * Date: 10/9/12
 * Time: 9:59 AM
 * To change this template use File | Settings | File Templates.
 */
object DNAProfiler {

  def transposeStrings(strings:List[String]): List[String] = {
    val sizeOfSequences = strings.size
    val numberOfSequences = strings(0).size

    val ls = for (j<-List.range(0,numberOfSequences); i<-List.range(0,sizeOfSequences))
      yield strings(i).charAt(j)

    val splitted = ls.mkString.sliding(sizeOfSequences,sizeOfSequences)
    return splitted.toList
  }

  def profileSequence(sequence : String) : (Int,Int,Int,Int) = {
    var a:Int = 0
    var c:Int = 0
    var g:Int = 0
    var t:Int = 0
    sequence.toCharArray.foreach( _ match {
      case 'A'=> a=a+1
      case 'C'=> c=c+1
      case 'G'=> g=g+1
      case 'T'=> t=t+1
    })
    return (a,c,g,t)
  }

  def profile(sequences : List[String]) : List[(Int,Int,Int,Int)] = {
    for (sequence <- sequences)
      yield profileSequence(sequence)
  }

  def consensus(profile : List[(Int,Int,Int,Int)]): String = {
    val listIndexes = for (sequence <- profile)
                        yield sequence match {
                          case (a,b,c,d) => List(a,b,c,d).indexOf(List(a,b,c,d).max)
                        }

    val s = for (nBase <- listIndexes)
      yield nBase match {
      case 0 => "A"
      case 1 => "C"
      case 2 => "G"
      case 3 => "T"
    }
    return s.mkString

  }
}
