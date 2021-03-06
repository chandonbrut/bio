package rosalind.algo

import rosalind.util.KMER

/**
 * Created by Jonas on 07/12/13.
 */

case class Edge(val kmer : KMER, val node1 : KMER, val node2: KMER) {
  def equals(other : Edge) : Boolean =  {
    (this.kmer equals other.kmer) &&
      (this.node1 equals other.node1) &&
      (this.node2 equals other.node2)
  }
}

case class Edge2(val kmer : (KMER,KMER), val node1 : (KMER,KMER), val node2: (KMER,KMER)) {
  def equals(other : Edge2) : Boolean =  {
    (this.kmer equals other.kmer) &&
      (this.node1 equals other.node1) &&
      (this.node2 equals other.node2)
  }
}
