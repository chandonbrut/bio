package rosalind.util

import rosalind.algo.{Edge, Node, GenericGraph}
import java.io.File
import scala.collection.mutable.ListBuffer

/**
 * Created by Jonas on 08/12/13.
 */
trait GraphReader {
  def read(file : File) : GenericGraph = {
    val lines = io.Source.fromFile(file).getLines()
    var nodes = new ListBuffer[Node]()
    var edges = new ListBuffer[Edge]()
    for (l<-lines; flds = l.split(" ")) {
      nodes appendAll List(new Node(new KMER(flds(0))))
      edges appendAll {
        for (target <- flds(2).split(","))
        yield new Edge(new KMER("edge"),new KMER(flds(0)),new KMER(target))
      }.toList
    }
    return new GenericGraph(nodes,edges)
  }
}
