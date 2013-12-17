package rosalind.algo

import rosalind.util.KMER
import scala.collection.mutable.ListBuffer

/**
 * Created by Jonas on 07/12/13.
 */
case class Node(val label : KMER) {
  def equals(other: Node) : Boolean = label equals other.label
  val edges = new ListBuffer[Edge]
  def addEdge(e : Edge) : Edge = {
    edges append e
    e
  }
  def delEdge(e : Edge) : Edge = {
    edges remove edges.indexOf(e)
  }
}

case class Node2(val c : (KMER,KMER)) {
  def equals(other: Node2) : Boolean = label equals other.label
  val edges = new ListBuffer[Edge2]
  def addEdge(e : Edge2) : Edge2 = {
    edges append e
    e
  }
  def delEdge(e : Edge2) : Edge2 = {
    edges remove edges.indexOf(e)
  }
  val label = c._1.content + c._2.content
}