package rosalind.algo

import rosalind.util.KMER
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks
import scala.collection.mutable


/**
 * Created by Jonas on 07/12/13.
 */
object Graph {

  def deBruijnAdj(kmers : List[KMER]) : AdjGraph = {

    val adj = new mutable.HashMap[Node,ListBuffer[Edge]]()

    for (kmer <- kmers) {
      val _n1 = new Node(kmer.prefix())
      val _n2 = new Node(kmer.suffix())

      adj.get(_n1).getOrElse(adj.put(_n1,new ListBuffer[Edge]))
      val _ln1 = adj.get(_n1).get
      _ln1.append(new Edge(kmer, kmer.prefix, kmer.suffix))

      adj.get(_n2).getOrElse(adj.put(_n2,new ListBuffer[Edge]))
      val _ln2 = adj.get(_n2).get
      _ln2.append(new Edge(kmer, kmer.prefix, kmer.suffix))
    }

    return new AdjGraph(adj)
  }

  def deBruijn(kmers : List[KMER]) : GenericGraph = {

    val adj = new mutable.HashMap[Node,ListBuffer[Edge]]()

    for (kmer <- kmers) {
      val _n1 = new Node(kmer.prefix())
      val _n2 = new Node(kmer.suffix())

      adj.get(_n1).getOrElse(adj.put(_n1,new ListBuffer[Edge]))
      val _ln1 = adj.get(_n1).get
      _ln1.append(new Edge(kmer, kmer.prefix, kmer.suffix))

      adj.get(_n2).getOrElse(adj.put(_n2,new ListBuffer[Edge]))
      val _ln2 = adj.get(_n2).get
      _ln2.append(new Edge(kmer, kmer.prefix, kmer.suffix))
    }

    val _n = new ListBuffer[Node]
    val _e = new ListBuffer[Edge]

    return new GenericGraph(_n,_e)
  }

  def eulerianPath(graph : GenericGraph) : List[Node] = {
    var odd = List[Node]()

    for (node <- graph.nodes) {
      val indegree = graph.edgesTo(node).size
      val outdegree =  graph.edgesFrom(node).size
      if (indegree != outdegree) {
        odd = odd ::: List(node)
      }
    }
    println(odd.size+ " odds ")
    var cycle = List[Node]()
    var from : Node = graph.nodes(0)
    var to : Node = graph.nodes(0)

    if (odd.size == 2) {

      val n1bal = graph.edgesTo(odd(0)).size - graph.edgesFrom(odd(0)).size

      from = odd(0)
      to = odd(1)

      if (n1bal > 0) {
        from = odd(1)
        to = odd(0)
      }

    }



    var insertPosition = 0

    while (graph.edges.size > 0) {
      val way = graph.findWay(from,to)
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
    println(odd.size+ " odds ")

    if (odd.size > 2) {
      throw new IllegalArgumentException("Illegal graph, more than 2 odd vertexes.")
    }

    val edges = new collection.mutable.ListBuffer[Edge]()
    edges appendAll graph.edges


    var from : Node = graph.nodes(0)
    var to : Node = from

    if(odd.size == 2) {
      from = odd(0)
      to = odd(0)
    }

    if (odd.size == 1) {
      from = odd(0)
      to = odd(0)
    }

    var cycle = List[Node]()

    var insertPosition = 0

    val maxE = graph.edges.size

      while (graph.edges.size > 0) {
        val prc : Int = graph.edges.size * 100 / maxE
        if ((prc % 2) == 1) {
          println(prc)
        }
        val way = graph.findWay(from,to)
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
      val n1i = nodes.indexOf(n1)
      if (n1i != -1) {
        nodes.remove(n1i)
      }

    }
    if(edges(n2).size == 0){
      val n2i = nodes.indexOf(n2)
      if (n2i != -1) {
        nodes.remove(n2i)
      }
    }
  }

  def edges(node : Node) : List[Edge] = {
    for (edge <- edges.toList; if ((edge.node1 equals node.label) || (edge.node2 equals node.label))) yield edge
  }

  def edgesFrom(node : Node) : List[Edge] = {
    { for (edge <- edges.toList; if (edge.node1 equals node.label)) yield edge }.sortBy(_.kmer.content)
  }

  def edgesTo(node : Node) : List[Edge] = {
    { for (edge <- edges.toList; if (edge.node2 equals node.label)) yield edge }.sortBy(_.kmer.content)
  }


  def findWay(from : Node, to : Node) : List[Node] = {
    var way = List[Node]()
    var current = from
    do {
      way = way ::: List(current)
      val nextEdge = this.edgesFrom(current)(0)
      current = new Node(nextEdge.node2)
      removeEdge(nextEdge)
    } while(current != to)
    if (!(from equals to)){
      way = way ::: List(to)
    }
    return way.toList
  }

}

case class AdjGraph(val adj : mutable.HashMap[Node,ListBuffer[Edge]]) {

  def nodes() : List[Node] = {
    adj.keySet.toList.sortBy(_.label.content)
  }

  def edges() : List[Edge] = {
    adj.values.flatten.toList
  }

  def eulerianCycle() : List[Node] = {
    var odd = List[Node]()
    for (node <- this.nodes) {
      val grad = adj.get(node).get.size
      if (grad == 0) {
        throw new IllegalArgumentException("Illegal graph, isolated vertex.")
      } else if(grad % 2 == 1) {
        odd = odd ::: List(node)
      }
    }
    println(odd.size+ " odds ")

    if (odd.size > 2) {
      throw new IllegalArgumentException("Illegal graph, more than 2 odd vertexes.")
    }


    var from : Node = nodes()(0)
    var to : Node = from

    if(odd.size == 2) {
      from = odd(0)
      to = odd(0)
    }

    if (odd.size == 1) {
      from = odd(0)
      to = odd(0)
    }

    var cycle = List[Node]()

    var insertPosition = 0

    while (this.edges.size > 0) {

      val way = findWay(from,to)
      val firstPart= cycle.slice(0,insertPosition)
      val  secondPart = cycle.slice(insertPosition,insertPosition+cycle.size)
      cycle = firstPart ::: way ::: secondPart
      Breaks.breakable {
        for (v<-cycle) {
          if(nodes.contains(v)) {
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


  def removeEdge(edge : Edge) = {

    val n1 = new Node(edge.node1);
    val n2 = new Node(edge.node2);

    val l1 = adj.get(n1).get
    val l2 = adj.get(n2).get

    l1.remove(l1.indexOf(edge))
    l2.remove(l2.indexOf(edge))

    if (l1.size == 0)
      adj.remove(n1)

    if (l2.size ==0)
      adj.remove(n2)

  }

  def edges(node : Node) : List[Edge] = {
    return adj.get(node).get.toList
  }

  def edgesFrom(node : Node) : List[Edge] = {
    { for (edge <- this.edges(node); if (edge.node1 equals node.label)) yield edge }.sortBy(_.node2.content)
  }

  def edgesTo(node : Node) : List[Edge] = {
    { for (edge <- this.edges(node); if (edge.node2 equals node.label)) yield edge }.sortBy(_.kmer.content)
  }


  def findWay(from : Node, to : Node) : List[Node] = {
    var way = List[Node]()
    var current = from
    do {
      way = way ::: List(current)
      val nextEdge = this.edgesFrom(current)(0)
      current = new Node(nextEdge.node2)
      removeEdge(nextEdge)
    } while(current != to)
    if (!(from equals to)){
      way = way ::: List(to)
    }
    return way.toList
  }

}