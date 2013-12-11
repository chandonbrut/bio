package rosalind.solutions.coursera

import javax.swing.{JFileChooser, JComponent}
import rosalind.util.GraphReader
import rosalind.algo.Graph

/**
Sample Input:
     0 -> 2
     1 -> 3
     2 -> 1
     3 -> 0,4
     6 -> 3,7
     7 -> 8
     8 -> 9
     9 -> 6

Sample Output:
     6->7->8->9->6->3->0->2->1->3->4

 */
object W5L1_EULERIAN_PATH extends JComponent with App with GraphReader {
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

  println(labels.mkString("->"))

}