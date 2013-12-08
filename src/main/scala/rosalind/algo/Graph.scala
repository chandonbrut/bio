package rosalind.algo

import rosalind.util.KMER
import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks


/**
 * Created by Jonas on 07/12/13.
 */
object Graph {

  def deBruijn(kmers : List[KMER]) : GenericGraph = {
    val _nodes = { for (edge <- kmers) yield List(new Node(edge.prefix()), new Node(edge.suffix()) ) }.flatten.distinct.sortBy(_.label.content)

    val nodes  = new ListBuffer[Node]
    nodes appendAll _nodes

    val edges  = new ListBuffer[Edge]
    for (kmer <- kmers; node <- nodes; if ((kmer.suffix equals node.label)))
    edges append new Edge(kmer, kmer.prefix, node.label)


    return new GenericGraph(nodes,edges)
  }

  def eulerianCycle(graph : GenericGraph) : List[Node] = {
    var odd = List[Node]()
    for (node <- graph.nodes) {
      val grad = graph.edges(node).size
      if (grad == 0) {
        throw new IllegalArgumentException("Illegal graph, isolated vertex.")
      } else if(grad % 2 == 1) {
        odd = odd ::: List(node)
      }
    }

    if (odd.size > 2) {
      throw new IllegalArgumentException("Illegal graph, more than 2 odd vertexes.")
    }

    val edges = new collection.mutable.ListBuffer[Edge]()
    edges appendAll graph.edges


    var from : Node = graph.nodes(0)
    var to : Node = from

    if(odd.size == 2) {
      from = odd(0)
      to = odd(1)
    }

    println("Odd size " + odd.size)

    var cycle = List[Node]()

    var insertPosition = 0

      while (graph.edges.size > 0) {
        //      println("finding way from " + from + " to " + to)
        val way = graph.findWay(from,to)
        //      println(way.mkString("->"))
        val firstPart= cycle.slice(0,insertPosition)
        val  secondPart = cycle.slice(insertPosition,insertPosition+cycle.size)
        cycle = firstPart ::: way ::: secondPart
        Breaks.breakable {
          for (v<-cycle) {
          if(graph.nodes.contains(v)) {
            from = v
            to = v
            insertPosition = cycle.indexOf(v)
            Breaks.break()
          }
        }
      }
    }

    return cycle
  }


}

case class GenericGraph(val nodes : ListBuffer[Node], val edges : ListBuffer[Edge]){

  def removeEdge(edge : Edge) = {
    edges.remove(edges.indexOf(edge))
    val n1 = new Node(edge.node1);
    val n2 = new Node(edge.node2);
    if(edges(n1).size == 0){
      nodes.remove(nodes.indexOf(n1))
    }
    if(edges(n2).size == 0){
      nodes.remove(nodes.indexOf(n2))
    }
  }

  def edges(node : Node) : List[Edge] = {
    for (edge <- edges.toList; if ((edge.node1 equals node.label) || (edge.node2 equals node.label))) yield edge
  }

  def edgesFrom(node : Node) : List[Edge] = {
    for (edge <- edges.toList; if (edge.node1 equals node.label)) yield edge
  }

  def edgesTo(node : Node) : List[Edge] = {
    for (edge <- edges.toList; if (edge.node1 equals node.label)) yield edge
  }


  def findWay(from : Node, to : Node) : List[Node] = {
    var way = List[Node]()
    var current = from
    do {
      way = way ::: List(current)
      val nextEdge = this.edgesFrom(current)(0)
      removeEdge(nextEdge)
      current = new Node(nextEdge.node2)
    } while(current != to)
    if (!(from equals to)){
      way = way ::: List(to)
    }
    return way.toList
  }

}