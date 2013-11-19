package br.mil.eb.ime.rosalind.algo
import scala.collection.mutable
import br.mil.eb.ime.rosalind.util.DNA

/**
 * Created with IntelliJ IDEA.
 * User: jonas
 * Date: 10/9/12
 * Time: 9:59 AM
 * To change this template use File | Settings | File Templates.
 */
object DNAProfiler {

  def transposeStrings(strings:List[String]): List[String] = {
    val sizeOfSequences = strings.size
    val numberOfSequences = strings(0).size

    val ls = for (j<-List.range(0,numberOfSequences); i<-List.range(0,sizeOfSequences))
      yield strings(i).charAt(j)

    val splitted = ls.mkString.sliding(sizeOfSequences,sizeOfSequences)
    return splitted.toList
  }

  def profileSequence(sequence : String) : (Int,Int,Int,Int) = {
    var a:Int = 0
    var c:Int = 0
    var g:Int = 0
    var t:Int = 0
    sequence.toCharArray.foreach( _ match {
      case 'A'=> a=a+1
      case 'C'=> c=c+1
      case 'G'=> g=g+1
      case 'T'=> t=t+1
    })
    return (a,c,g,t)
  }

  def profile(sequences : List[String]) : List[(Int,Int,Int,Int)] = {
    for (sequence <- sequences)
      yield profileSequence(sequence)
  }


  def countKMersWithReverseComplement(k : Int, sequence : String, tolerance : Int) : mutable.Map[String,Int] = {

    val kMers = sequence.sliding(k)
    val possibleKmers = generate(k)
    val allComplements = (for (possible <- possibleKmers) yield  (new DNA("unn",possible)).reverseComplement.sequence).toList

    val possibleAndComplement = possibleKmers zip allComplements

    val count = new mutable.HashMap[String,Int]

    for (k1 <- kMers; k2 <- possibleKmers)
      if ((Hamming.calculate(k1,k2) <= tolerance)) {
        if (count.get(k2) == None) {
          count.put(k2,1)
        } else {
          var c:Int = count.get(k2).get
          count.put(k2,(c+1))
        }
      }


    for (pc <- possibleAndComplement) {
      val v1 = count.get(pc._1)
      val v2 = count.get(pc._2)
      var vv = 0
      if (v1 != None) {
         vv = v1.get
      }
      var yy = 0
      if (v2 != None) {
        yy = v2.get
      }
      count.put(pc._1, vv + yy)
      count.put(pc._2, vv + yy)

    }


    return count
  }

  def countKMers(k : Int, sequence : String, tolerance : Int) : mutable.Map[String,Int] = {
    val kMers = sequence.sliding(k)
    val possibleKmers = generate(k)
    val count = new mutable.HashMap[String,Int]

    for (k1 <- kMers; k2 <- possibleKmers)
      if (Hamming.calculate(k1,k2) <= tolerance)
        if (count.get(k2) == None) {
          count.put(k2,1)
        } else {
          var c:Int = count.get(k2).get
          count.put(k2,(c+1))
        }
    return count
  }

  def generate(k : Int) : List[String] = {
    val possibleBases = List("A","C","G","T")
    if (k>1) {
      val yeah = for (prefix <- possibleBases; suffix <- generate(k-1))
      yield prefix + suffix
      return yeah
    }
    if (k == 1) {
      return possibleBases
    }
    return List()
  }

  def countKMers(k : Int, sequence : String) : mutable.Map[String,Int] = {
    val kMers = sequence.sliding(k)
    val count = new mutable.HashMap[String,Int]

    for (k <- kMers)
      if (count.get(k) == None) {
        count.put(k,1)
      } else {
        var c:Int = count.get(k).get
        count.put(k,(c+1))
      }
    return count

  }

  def consensus(profile : List[(Int,Int,Int,Int)]): String = {
    val listIndexes = for (sequence <- profile)
                        yield sequence match {
                          case (a,b,c,d) => List(a,b,c,d).indexOf(List(a,b,c,d).max)
                        }

    val s = for (nBase <- listIndexes)
      yield nBase match {
      case 0 => "A"
      case 1 => "C"
      case 2 => "G"
      case 3 => "T"
    }
    return s.mkString

  }
}