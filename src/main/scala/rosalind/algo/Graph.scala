package rosalind.algo

import rosalind.util.KMER
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks
import scala.collection.mutable


/**
 * Created by Jonas on 07/12/13.
 */
object Graph {

  def dag(str : String) : DAG = {

    val data = str.split("\n")
    val src = data.head.toInt
    val snk = data.tail.head.toInt
    val es ={  for (l <- data.tail.tail; mm = l.split("->"); targetAndWeight = mm(1).split(":") )
    yield (mm(0).toInt,targetAndWeight(0).toInt,targetAndWeight(1).toInt)}.toSet

    val ns:List[Int] = {
      for (e<-es) yield e._1
    }.toList ::: { for (e<-es) yield e._2 }.toList

    return new DAG(src,snk,ns.toSet[Int],es)
  }

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

  def findNonBranching() : List[List[Node]] = {

    val contigs = new mutable.ListBuffer[List[Node]]()
    val stack = new collection.mutable.Stack[(Node,Node)]()
    val visited = new mutable.HashSet[Node]()
    for (node <- this.nodes) {
      val in = edgesTo(node)
      val out = edgesFrom(node)

      if (in.size < out.size) {

        for (o <- out) {
          stack.push((node,new Node(o.node2)))
        }
        visited add node
      }
    }

    while (stack nonEmpty) {

      val popped = stack pop

      var contigPath = List(popped._1, popped._2)
      var node = contigPath.last

      while (( (edgesFrom(node).size == 1) &&
        (edgesTo(node).size == 1) &&
        (!visited.contains(node))  )) {
        visited add node
        node = new Node(edgesFrom(node)(0).node2)
        contigPath = contigPath ::: List(node)
      }
      contigs append contigPath

      if (!visited.contains(node)) {
        for (o <- edgesFrom(node)) {
          stack.push((node,new Node(o.node2)))
        }
        visited add node

      }
    }

    return contigs.toList
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
      print(v.charAt(i-1))
    }
  }
}

case class ScoredAlignmentGraph(val v : String, val w : String, val scores : List[(String,String,Int)], val indelPenalty : Int) {
  private val data = genData()._1
  private val backtracking = genData()._2

  def genData() : (Array[Array[Int]], Array[Array[String]])  = {
    val n = v.size+1
    val m = w.size+1

    val d = Array.ofDim[Int](n,m)
    val b = Array.ofDim[String](n,m)

    d(0).update(0,0)
    b(0).update(0,"nul")


    for (x <- List.range(1,n)) {
      d(x).update(0,d(x-1)(0)-5)
      b(x).update(0,"dow")
    }


    for (x <- List.range(1,m)) {
      d(0).update(x,d(0)(x-1)-5)
      b(0).update(x,"rig")
    }

    val s = new mutable.HashMap[String,Int]
    for (sc <- scores)
      s(sc._1 + sc._2) = sc._3

    for (i<-List.range(1,n); j<-List.range(1,m)) {

      val matchOrMismatch =  d(i-1)(j-1) + s(v.charAt(i-1)+""+w.charAt(j-1))

      val values = List(
        d(i-1)(j) - indelPenalty,
        d(i)(j-1) - indelPenalty,
        matchOrMismatch
      )
      d(i)(j) = values.max
      if (d(i)(j) == d(i-1)(j) - indelPenalty) {
        b(i)(j) = "dow"
      } else if (d(i)(j) == d(i)(j-1) - indelPenalty) {
        b(i)(j) = "rig"
      } else if (d(i)(j) == matchOrMismatch) {
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

  def output(i : Int, j : Int, accV : List[String], accW : List[String]) : Unit = {
    if ((i==0) && (j==0)) {
      println(data(v.size)(w.size))
      println(accV.mkString(""))
      println(accW.mkString(""))
    } else if(backtracking(i)(j) == "dow") {
      val av = List(v.charAt(i-1).toString)  ::: accV
      val aw = List("-")  ::: accW
      output(i-1,j,av,aw)
    } else if (backtracking(i)(j) == "rig") {
      val av = List("-")  ::: accV
      val aw = List(w.charAt(j-1).toString)  ::: accW
      output(i,j-1,av,aw)
    } else {
      val av = List(v.charAt(i-1).toString)  ::: accV
      val aw = List(w.charAt(j-1).toString)  ::: accW
      output(i-1,j-1,av,aw)
//      print(v.charAt(i-1))
    }
  }
}

case class FittingAlignmentGraph(val v : String, val w : String, val indelPenalty : Int, val matchReward : Int, val mismatchPenalty : Int) {
  private val data = genData()._1
  private val backtracking = genData()._2

  def genData() : (Array[Array[Int]], Array[Array[String]])  = {
    val n = v.size+1
    val m = w.size+1

    val d = Array.ofDim[Int](n,m)
    val b = Array.ofDim[String](n,m)

    d(0).update(0,0)
    b(0).update(0,"nul")


    for (x <- List.range(1,n)) {
      d(x).update(0,0)
      b(x).update(0,"dow")
    }


    for (x <- List.range(1,m)) {
      d(0).update(x,d(0)(x-1) - indelPenalty)
      b(0).update(x,"rig")
    }

  for (i<-List.range(1,n); j<-List.range(1,m)) {

      val s = if (v.charAt(i-1) == w.charAt(j-1)) {
        matchReward
      } else {
        -mismatchPenalty
      }
      val matchOrMismatch =  d(i-1)(j-1) + s

      val values = List(
        matchOrMismatch,
        d(i-1)(j) - indelPenalty,
        d(i)(j-1) - indelPenalty
      )
      d(i)(j) = values.max
      if (d(i)(j) == d(i-1)(j) - indelPenalty) {
        b(i)(j) = "dow"
      } else if (d(i)(j) == d(i)(j-1) - indelPenalty) {
        b(i)(j) = "rig"
      } else if (d(i)(j) == s) {
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

  def output(i : Int, j : Int, accV : List[String], accW : List[String]) : Unit = {
    if ((i==0) || (j==0)) {
      val maxNode = getEndNode()
      println(data(maxNode._1)(maxNode._2))
      println(accV.mkString(""))
      println(accW.mkString(""))
    } else if(backtracking(i)(j) == "dow") {
      val av = List(v.charAt(i-1).toString)  ::: accV
      val aw = List("-")  ::: accW
      output(i-1,j,av,aw)
    } else if (backtracking(i)(j) == "rig") {
      val av = List("-")  ::: accV
      val aw = List(w.charAt(j-1).toString)  ::: accW
      output(i,j-1,av,aw)
    } else {
      val av = List(v.charAt(i-1).toString)  ::: accV
      val aw = List(w.charAt(j-1).toString)  ::: accW
      output(i-1,j-1,av,aw)
      //      print(v.charAt(i-1))
    }
  }

  def getEndNode() : (Int,Int) = {
    var maxI = v.size
    var maxJ = w.size
    var maxVal = Int.MinValue

    for (i<-List.range(1,v.size+1))
      if (data(i)(w.size) > maxVal) {
        maxVal = data(i)(w.size)
        maxI = i
        maxJ = w.size
      }


    for (i<-List.range(1,v.size+1); j<-List.range(1,w.size); if (data(i)(j) == maxVal))
      println("Max val reached")

    return (maxI,maxJ)

  }
}

case class ScoredLocalAlignmentGraph(val v : String, val w : String, val scores : List[(String,String,Int)], val indelPenalty : Int) {
  private val data = genData()._1
  private val backtracking = genData()._2

  def genData() : (Array[Array[Int]], Array[Array[String]])  = {
    val n = v.size+1
    val m = w.size+1

    val d = Array.ofDim[Int](n,m)
    val b = Array.ofDim[String](n,m)

    d(0).update(0,0)
    b(0).update(0,"nul")


    for (x <- List.range(1,n)) {
      d(x).update(0,0)
      b(x).update(0,"dow")
    }


    for (x <- List.range(1,m)) {
      d(0).update(x,0)
      b(0).update(x,"rig")
    }

    val s = new mutable.HashMap[String,Int]
    for (sc <- scores)
      s(sc._1 + sc._2) = sc._3

    for (i<-List.range(1,n); j<-List.range(1,m)) {

      val matchOrMismatch =  d(i-1)(j-1) + s(v.charAt(i-1)+""+w.charAt(j-1))

      val values = List(
        matchOrMismatch,
        d(i-1)(j) - indelPenalty,
        d(i)(j-1) - indelPenalty,
        0
      )
      d(i)(j) = values.max
      if (d(i)(j) == d(i-1)(j) - indelPenalty) {
        b(i)(j) = "dow"
      } else if (d(i)(j) == d(i)(j-1) - indelPenalty) {
        b(i)(j) = "rig"
      } else if (d(i)(j) == 0) {
        b(i)(j) = "tax"
      } else if (d(i)(j) == matchOrMismatch) {
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

  def output(i : Int, j : Int, accV : List[String], accW : List[String]) : Unit = {
    if ((i==0) && (j==0)) {
      val maxNode = getMaxNode()
      println(data(maxNode._1)(maxNode._2))
      println(accV.mkString(""))
      println(accW.mkString(""))
    } else if(backtracking(i)(j) == "dow") {
      val av = List(v.charAt(i-1).toString)  ::: accV
      val aw = List("-")  ::: accW
      output(i-1,j,av,aw)
    } else if (backtracking(i)(j) == "rig") {
      val av = List("-")  ::: accV
      val aw = List(w.charAt(j-1).toString)  ::: accW
      output(i,j-1,av,aw)
    } else if (backtracking(i)(j) == "tax") {
      output(0,0,accV,accW)
    } else {
      val av = List(v.charAt(i-1).toString)  ::: accV
      val aw = List(w.charAt(j-1).toString)  ::: accW
      output(i-1,j-1,av,aw)
      //      print(v.charAt(i-1))
    }
  }

  def getMaxNode() : (Int,Int) = {
    var maxI = v.size
    var maxJ = w.size
    var maxVal = data(maxI)(maxJ)

    for (i<-List.range(1,v.size+1); j<-List.range(1,w.size))
      if (data(i)(j) >= maxVal) {
        maxVal = data(i)(j)
        maxI = i
        maxJ = j
      }


    for (i<-List.range(1,v.size+1); j<-List.range(1,w.size); if (data(i)(j) == maxVal))
      println("Max val reached")

    return (maxI,maxJ)

  }
}
case class DAG(val source : Int, val sink : Int, val nodes : Set[Int], val edges : Set[(Int,Int,Int)]) {

  def topologicalOrdering() : List[Int] = {
    var list = new ListBuffer[Int]()
    var candidates = new ListBuffer[Int]

    var removableEdges = new ListBuffer[(Int,Int,Int)]
    removableEdges appendAll edges

    val incoming = for (e<-edges) yield e._2

    for (n<-nodes; if (!incoming.contains(n)))
      candidates append n

    while (!candidates.isEmpty) {
      val c = candidates.remove(0)
      list append c
      for (e<-removableEdges; if (e._1 == c)) {
        val removed = removableEdges.remove(removableEdges.indexOf(e))
        val incomingA = for (ie<-removableEdges; if(ie._2 == removed._2)) yield ie
        if (incomingA.size == 0) {
          candidates append removed._2
        }
      }
    }

    return list.toList

  }

  def longestPath() : Unit  = {


    val d = new mutable.HashMap[Int,Int]()
    val b = new mutable.HashMap[Int,Int]()



    for (x <- nodes)
      d(x) = 0


    val order = this.topologicalOrdering()

    println("order:" + order.mkString(" "))
    var path = new ListBuffer[Int]
    val beg = order.indexOf(source)
    val end = order.indexOf(sink)

    val validNodes = order.slice(beg,end+1)
    val validEdges = for (e<-edges; if(validNodes.contains(e._1) && (validNodes.contains(e._2)))) yield e

    for (a<-validNodes) {
      if(a != source) {
        println("on node " + a)
        val nodesLeadingToNode = validEdges.filter(_._2 == a)
        val ws = for (e<-nodesLeadingToNode) yield (e._1,d(e._1) + e._3)
        if (ws.size > 0) {
          d(a) = ws.maxBy(_._2)._2
          b(a) = ws.maxBy(_._2)._1
        } else {
          d(a) = Int.MinValue
        }
      } else {
        d(a) = 0
      }
    }

    var toGo = new mutable.Stack[Int]()

    toGo.push(sink)
    while (!toGo.isEmpty) {
      val n = toGo.pop()
      path append n

      if (n != source) {
        try {
          toGo.push(b(n))
        } catch {
          case _ => println("Erro em " +n)
        }
      }
    }

  println(d)
    println("---")

    println (d(sink))
    println (path.toList.reverse.mkString("->"))
  }
}