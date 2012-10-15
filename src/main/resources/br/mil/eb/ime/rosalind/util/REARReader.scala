package br.mil.eb.ime.rosalind.util

import io.Source

/**
 * Created with IntelliJ IDEA.
 * User: jonas
 * Date: 10/9/12
 * Time: 2:30 PM
 * To change this template use File | Settings | File Templates.
 */
object REARReader {
  def readFromFile(path:String):List[(List[Int],List[Int])] = {
    val lines = Source.fromFile(path).getLines.toList
    val (ls1,rems1) = lines.splitAt(2)
    val (ls2,rems2) = rems1.tail.splitAt(2)
    val (ls3,rems3) = rems2.tail.splitAt(2)
    val (ls4,rems4) = rems3.tail.splitAt(2)
    val (ls5,rems5) = rems4.tail.splitAt(2)
    val toProcess = List(ls1,ls2,ls3,ls4,ls5)

    val perms = for (l<-toProcess)
      yield (l(0).split(" ").toList.map(_.toInt),l(1).split(" ").toList.map(_.toInt))

    println(perms.toList)

    return perms.toList

  }

}
