package rosalind.algo

import rosalind.util.KMER
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks
import scala.collection.mutable


/**
 * Created by Jonas on 07/12/13.
 */
object Graph {

  def deBruijnAdj2(kmers : List[(KMER,KMER)]) : AdjGraph2 = {

    val adj = new mutable.HashMap[Node2,ListBuffer[Edge2]]()

    for (kmer <- kmers) {
      val _n1 = new Node2((kmer._1.prefix(),kmer._2.prefix()))
      val _n2 = new Node2((kmer._1.suffix(),kmer._2.suffix()))

      adj.get(_n1).getOrElse(adj.put(_n1,new ListBuffer[Edge2]))
      val _ln1 = adj.get(_n1).get
      _ln1.append(new Edge2(kmer, (kmer._1.prefix(),kmer._2.prefix()), (kmer._1.suffix(),kmer._2.suffix())))

      adj.get(_n2).getOrElse(adj.put(_n2,new ListBuffer[Edge2]))
      val _ln2 = adj.get(_n2).get
      _ln2.append(new Edge2(kmer, (kmer._1.prefix(),kmer._2.prefix()), (kmer._1.suffix(),kmer._2.suffix())))
    }

    return new AdjGraph2(adj)
  }

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



case class AdjGraph2(val adj : mutable.HashMap[Node2,ListBuffer[Edge2]]) {
  def nodes() : List[Node2] = {
    adj.keySet.toList
  }
  def edges() : List[Edge2] = {
    adj.values.flatten.toList
  }
  def eulerianCycle() : List[Node2] = {
    var odd = List[Node2]()
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


    var from : Node2 = nodes()(0)
    var to : Node2 = from

    if(odd.size == 2) {

      if (edgesFrom(odd(0)).size > 0) {
        from = odd(0)
        to = odd(1)
      } else {
        from = odd(1)
        to = odd(0)
      }
    }

    if (odd.size == 1) {
      from = odd(0)
      to = odd(0)
    }

    var cycle = List[Node2]()

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


  def removeEdge(edge : Edge2) = {

    val n1 = new Node2(edge.node1);
    val n2 = new Node2(edge.node2);

    val l1 = adj.get(n1).get
    val l2 = adj.get(n2).get

    l1.remove(l1.indexOf(edge))
    l2.remove(l2.indexOf(edge))

    if (l1.size == 0)
      adj.remove(n1)

    if (l2.size ==0)
      adj.remove(n2)

  }

  def edges(node : Node2) : List[Edge2] = {
    return adj.get(node).get.toList
  }

  def edgesFrom(node : Node2) : List[Edge2] = {
    { for (edge <- this.edges(node); if (edge.node1 equals node.c)) yield edge }
  }

  def edgesTo(node : Node2) : List[Edge2] = {
    { for (edge <- this.edges(node); if (edge.node2 equals node.c)) yield edge }
  }


  def findWay(from : Node2, to : Node2) : List[Node2] = {
    var way = List[Node2]()
    var current = from
    do {
      way = way ::: List(current)
      val nextEdge = this.edgesFrom(current)(0)
      current = new Node2(nextEdge.node2)
      removeEdge(nextEdge)
    } while(current != to)
    if (!(from equals to)){
      way = way ::: List(to)
    }
    return way.toList
  }
}

case class CityGraph(val n : Int, val m : Int, val down : Array[Array[Int]], val right : Array[Array[Int]]) {

  private val data = assembleMatrix()

  def assembleMatrix() : Array[Array[Int]] = {
    val d = Array.ofDim[Int](n+1,m+1)
    d(0)(0) = 0

    for (x <- List.range(1,n+1))
      d(0).update(x,d(0)(x-1) + right(0)(x-1))

    for (x <- List.range(1,n+1))
      d(x).update(0,d(x-1)(0) + down(x-1)(0))

    for (i <- List.range(1,n+1); j <- List.range(1,m+1)) {
      val w : Int = math.max(
        d(i-1)(j) + down(i-1)(j),
        d(i)(j-1) + right(i)(j-1)
      )
      d(i)(j) = w
    }

    return d
  }

  def print() = {
    for (d<-data)
      println(d.mkString(" "))
  }

  def southOrEast(i : Int, j : Int) : Int = {
    if (i == 0 && j == 0) {
      return 0
    }
    var x = Int.MaxValue
    var y = Int.MaxValue

    if (i > 0) {
      x = southOrEast(i-1,j) + data(i)(j)
    }

    if (j > 0) {
      y = southOrEast(i,j-1) + data(i)(j)
    }

    return math.max(x,y)

  }

  def maxWeight() : Int = {
    return data(n)(m)
  }

}

case class AlignmentGraph(val v : String, val w : String) {
  private val data = genData()._1
  private val backtracking = genData()._2

  def genData() : (Array[Array[Int]], Array[Array[String]])  = {
    val n = v.size+1
    val m = w.size+1

    val d = Array.ofDim[Int](n,m)
    val b = Array.ofDim[String](n,m)

    for (x <- List.range(0,n)) {
      d(x).update(0,0)
      b(x).update(0,"nul")
    }


    for (x <- List.range(0,m)) {
      d(0).update(x,0)
      b(0).update(x,"nul")
    }


    for (i<-List.range(1,n); j<-List.range(1,m)) {

        val matchOrMismatch = if (v.charAt(i-1) == w.charAt(j-1))
                                d(i-1)(j-1) + 1
                              else
                                d(i-1)(j-1)

        val values = List(
          d(i-1)(j),
          d(i)(j-1),
          matchOrMismatch
        )
        d(i)(j) = values.max
        if (d(i)(j) == d(i-1)(j)) {
          b(i)(j) = "dow"
        } else if (d(i)(j) == d(i)(j-1)) {
          b(i)(j) = "rig"
        } else if (d(i)(j) == (d(i-1)(j-1)+1)) {
          b(i)(j) = "dia"
        }
      }

    return (d,b)
  }

  def printData() = {
    for (d<-data)
      println(d.mkString(" "))
  }

  def printBacktrack() = {
    for (d<-backtracking)
      println(d.mkString(" "))
  }

  def outputLCS(i : Int, j : Int) : Unit = {
    if ((i==0) || (j==0)) {

    } else if(backtracking(i)(j) == "dow") {
      outputLCS(i-1,j)
    } else if (backtracking(i)(j) == "rig") {
      outputLCS(i,j-1)
    } else {
      outputLCS(i-1,j-1)
      print(v.charAt(i))
    }
  }

}