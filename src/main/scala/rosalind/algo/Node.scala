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