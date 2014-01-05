package rosalind.solutions.coursera

import rosalind.util.KMER
import rosalind.algo.Graph
import javax.swing.{JFileChooser, JComponent}

/**
 * Created by jonas on 12/17/13.
 */
/*
Sample Input:
ATG
ATG
TGT
TGG
CAT
GGA
GAT
AGA

Sample Output:
AGA ATG ATG CAT GAT TGGA TGT
AGA ATG ATG CAT GAT TGGA TGT
 */
object  W5L5_CONTIG_GENERATION extends JComponent with App {

  val file = new JFileChooser();
  file.showOpenDialog(this)


  val lines = io.Source.fromFile(file.getSelectedFile).getLines()

  val kmers = { for (i<-lines) yield new KMER(i) }.toList

  val deBruijn = Graph.deBruijnAdj(kmers)

  val commons = deBruijn.findNonBranching()

  for (c<-commons)
    println(c mkString " ")


  val result = new collection.mutable.ListBuffer[String]

  for (path <- commons) {
    var resulting = path.head.label
    for (l <- path.tail) {
      resulting = resulting append l.label
    }
    result append resulting.content
  }

  println(result.toList.sorted.mkString(" "))




}
     //CA 0/1 AT 2/2 TG 2/2 GT 1/0 GG 1/1 AG 0/1 GA 2/1