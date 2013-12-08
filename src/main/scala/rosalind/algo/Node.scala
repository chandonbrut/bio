package rosalind.algo

import rosalind.util.KMER

/**
 * Created by Jonas on 07/12/13.
 */
case class Node(val label : KMER) {
  def equals(other: Node) : Boolean = label equals other.label
}