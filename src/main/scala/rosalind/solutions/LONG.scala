package br.mil.eb.ime.rosalind.solutions

import io.Source
import javax.management.remote.rmi._RMIConnection_Stub
import collection.parallel.mutable
import scala.collection
import scala.collection

/**
 * Created with IntelliJ IDEA.
 * User: jonasferreira
 * Date: 10/8/12
 * Time: 7:57 PM
 * To change this template use File | Settings | File Templates.
 */
object LONG extends App {

  def getFit(word:String,list:List[String]) : List[String] = {
    return for (l<-list if (doWeFit(List(word,l)))) yield l
  }

  def naiveOverlapped(full:List[String]) : List[String] = {
    println(full.size)
    if (full.size == 1)
      return full
    if (full.size==2)
      return List(maxOverlappedString(full))

    val wordMap = new collection.mutable.HashMap[String,List[String]]()

    for (word<-full)
      wordMap(word) = getFit(word,full.filterNot(_==word))

    val wordsToGlue:List[List[String]] =
    { for (key<-wordMap.keys if wordMap(key).size==1)
      yield key :: wordMap(key) }.toList

    val glued=for (w<-wordsToGlue)
      yield maxOverlappedString(w)

    val toBeRemoved = wordsToGlue.flatten
    val l = full.filterNot(toBeRemoved.contains(_))
    val listToBePassed = l ::: glued.removeDuplicates
    return naiveOverlapped(listToBePassed)
  }

  def doWeFit(strings:List[String]) : Boolean = {
    val sta = strings(0)
    val stb = strings(1)

    val maxLength=if (List(sta.length,stb.length).min == -1) sta.length
    else (List(sta.length,stb.length).min)

    for (i<-(maxLength to (maxLength/2) by -1)) {
      if (stb.startsWith(sta.substring(sta.length-i))) {
        return true
      } else if (sta.startsWith(stb.substring(stb.length-i))) {
        return true
      }
    }
    return false
  }

  def maxOverlappedString(strings:List[String]) : String = {
    val sta = strings(0)
    val stb = strings(1)

    if(sta.contains(stb)) {
      return sta
    }

    if (stb.contains(sta)) {
      return stb
    }

    val maxLength=if (List(sta.length,stb.length).min == -1) sta.length
                  else (List(sta.length,stb.length).min)

    for (i<-(maxLength to 1 by -1)) {
      if (stb.startsWith(sta.substring(sta.length-i))) {
        val returnable = sta + stb.substring(i)
        return returnable
      } else if (sta.startsWith(stb.substring(stb.length-i))) {
        val returnable = stb + sta.substring(i)
        return returnable
      }

    }
    println("shit happened")
    return sta+stb
  }

  val sequences = Source.fromFile("/rosalind_long.txt").getLines().toList

  val be = naiveOverlapped(sequences)
  println(be.mkString)

}