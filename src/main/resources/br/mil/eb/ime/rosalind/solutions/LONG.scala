package br.mil.eb.ime.rosalind.solutions

import io.Source
import javax.management.remote.rmi._RMIConnection_Stub

/**
 * Created with IntelliJ IDEA.
 * User: jonasferreira
 * Date: 10/8/12
 * Time: 7:57 PM
 * To change this template use File | Settings | File Templates.
 */
object LONG extends App {

  def overlapAllStrings(dirtyLs:List[String]) : String = {
    val ls=dirtyLs
    println(ls.size)
    if (ls.size==1) {
      return ls.mkString
    } else if (ls.size ==2) {
      return maxOverlappedString(ls)
    } else if (ls.size>2) {
      val toCombine : List[String] = ls.filterNot(_=="")
      val combined : List[List[String]] = toCombine.combinations(2).toList
      val newWords : List[String] = { for (pair<-combined) yield maxOverlappedString(pair) }.toList
      return overlapAllStrings(newWords.filterNot(_==""))
    } else {
      println(ls.size)
      return maxOverlappedString(ls)
    }
  }

  def maxOverlappedString(strings:List[String]) : String = {
    val sta = strings(0)
    val stb = strings(1)

    val cond1 = sta.contains(stb.substring(0,(stb.length/2)-1))
    val cond2 = sta.contains(stb.substring((stb.length/2)-1))
    val cond3 = stb.contains(sta.substring((sta.length/2)-1))
    val cond4 = stb.contains(sta.substring(0,(sta.length/2)-1))
    if (cond1 || cond2 || cond3 || cond4) {

    } else {
      return ""
    }

    if (sta.contains(stb)) {
      return sta
    }
    if (stb.contains(sta)) {
      return stb
    }
    if (sta==stb){
      println("a blessing")
      return sta
    }
    var maxString = ""
    val maxLength=List(sta.length,stb.length).min
    for (i<-List.range(1,maxLength) if (maxString.length == 0)) {
      if ((maxString.length==0) && (stb.startsWith(sta.substring((sta.length-1)-i)))) {
        maxString = sta.substring(0,(sta.length-1)-i) + stb
      } else if ((maxString.length==0) && (sta.startsWith(stb.substring((stb.length-1)-i)))) {
        maxString = stb.substring(0,(stb.length-1)-i) + sta
      }
    }
    return maxString
  }

  val sequences = Source.fromFile("/home/jonas/Downloads/rosalind_long.txt").getLines().toList

  println(overlapAllStrings(sequences))

  /*
  ATTAGACCTGCCGGAATAC      ATTAGACCTGCCGGAATAC

   */

}