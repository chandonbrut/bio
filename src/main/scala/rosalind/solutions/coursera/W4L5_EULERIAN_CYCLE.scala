package rosalind.solutions.coursera

import rosalind.util.GraphReader
import javax.swing.{JComponent, JFileChooser}
import rosalind.algo.Graph
import scala.collection.mutable

/**
 * Created by Jonas on 08/12/13.
 */
/*
Sample Input:
     0 -> 3
     1 -> 0
     2 -> 1,6
     3 -> 2
     4 -> 2
     5 -> 4
     6 -> 5,8
     7 -> 9
     8 -> 7
     9 -> 6

Sample Output:
     6->8->7->9->6->5->4->2->1->0->3->2->6
 */
object W4L5_EULERIAN_CYCLE extends JComponent with App with GraphReader {
  val file = new JFileChooser();
  file.showOpenDialog(this)
  val graph = read(file.getSelectedFile)

//  for (node <- graph.nodes.distinct.sortBy(_.label.content); if(!graph.edgesFrom(node).isEmpty)) {
//    val targets = for (e<-graph.edgesFrom(node)) yield e.node2.content
//    println(node.label.content + " -> " + targets.distinct.sorted.mkString(","))
//  }

  val eulerianPath = Graph.eulerianCycle(graph)
  val eulerianCycle = eulerianPath ::: List(eulerianPath.head)
  val labels = for (l<-eulerianCycle) yield l.label

  println(labels.mkString("->"))

}
