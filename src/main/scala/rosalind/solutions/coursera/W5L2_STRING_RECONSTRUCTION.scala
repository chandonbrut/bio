package rosalind.solutions.coursera

import javax.swing.{JComponent, JFileChooser}
import rosalind.util.{KMER, GraphReader}
import rosalind.algo.Graph

/**
 * Created by Jonas on 09/12/13.
 */
/*
Sample Input:
     CTT -> TTA
     ACC -> CCA
     TAC -> ACC
     GGC -> GCT
     GCT -> CTT
     TTA -> TAC

Sample Output:
     GGCTTACCA
 */


object W5L2_STRING_RECONSTRUCTION extends JComponent with App with GraphReader {
val file = new JFileChooser();
file.showOpenDialog(this)
val graph = read(file.getSelectedFile)

//  for (node <- graph.nodes.distinct.sortBy(_.label.content); if(!graph.edgesFrom(node).isEmpty)) {
//    val targets = for (e<-graph.edgesFrom(node)) yield e.node2.content
//    println(node.label.content + " -> " + targets.distinct.sorted.mkString(","))
//  }

val eulerianPath = Graph.eulerianPath(graph)
val eulerianCycle = eulerianPath
val labels = for (l<-eulerianCycle) yield l.label
var resulting = labels.head


  for(label <- labels.tail)
    resulting = resulting append label

  println(resulting.content)
}