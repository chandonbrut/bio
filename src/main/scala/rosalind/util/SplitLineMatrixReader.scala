package rosalind.util

import java.io.File
import scala.collection.mutable.ListBuffer

/**
 * Created by Jonas on 24/12/13.
 */
trait SplitLineMatrixReader {

  def read(file : File) : List[(String,String,Int)] = {

    val input = io.Source.fromFile(file).getLines().toList

    val labels = input.head.replaceAll(" +"," ").split(" ")
    val values = new ListBuffer[(String,String,Int)]
    for (line <- input.tail;n = line.replaceAll(" +"," ").split(" ")) {
      val node = n(0)
      for (i <- List.range(1,n.size)) {
        val v = (node, labels(i), n(i).toInt)
        values append v
      }
    }

    return values.toList
  }

}
