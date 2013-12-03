package br.mil.eb.ime.rosalind.algo
import scala.collection.mutable
import br.mil.eb.ime.rosalind.util.DNA
import scala.util.Random

/**
 * Created with IntelliJ IDEA.
 * User: jonas
 * Date: 10/9/12
 * Time: 9:59 AM
 * To change this template use File | Settings | File Templates.
 */
object DNAProfiler {

  val bases = List('A', 'C', 'G', 'T')

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

  def generate(k : Int, sequence : String) : List[String] = {
    val possible = countKMers(k,sequence)
    val kmers = for(kmer <- possible; if (kmer._2 > 0))
    yield kmer._1

    return kmers.toList
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

  def countBases(bases : List[String]) : List[Int] = {
    var max = Array(0,0,0,0)
    for (base <- bases) {
      base match {
        case "A" => max(0) = max(0) + 1
        case "C" => max(1) = max(1) + 1
        case "G" => max(2) = max(2) + 1
        case "T" => max(3) = max(3) + 1
      }
    }
    return max.toList
  }

  def consensus(profile : List[String]): String = {

    val si = profile(0).size
    var ret = Array.ofDim[Int](si)

    for (i <- List.range(0,si)) {
      val bases = { for (sequence <- profile) yield sequence.substring(i,i+1) }.toList
      val maximized = countBases(bases)
      ret(i) = maximized.indexOf(maximized.max)
    }

    val s = for (nBase <- ret.toList)
    yield nBase match {
        case 0 => "A"
        case 1 => "C"
        case 2 => "G"
        case 3 => "T"
        case _ => ""
      }
    return s.mkString
  }

  def countWordWithMutations(sequences : List[String], word : String, numMutations : Int) : Boolean = {
    val res = for (sequence <- sequences; frag <- sequence.sliding(word.size); if(Hamming.calculate(frag,word) <= numMutations))
    yield (sequence,true)

    var result = true
    for (sequence <- sequences)
      result = result && res.contains((sequence,true))

    return result
  }

  def motifEnumeration(sequences : List[String], k : Int, numMutations : Int)  : List[String] = {
    val big = sequences.mkString("")
    //    val kmers = { for (kmer <- generate(k); if (countWordWithMutations(sequences,kmer,0))) yield kmer }.toList
    val kmers = generate(k)

    val result = for (k1 <- kmers; k2 <- kmers; if ((!k1.equals(k2) && Hamming.calculate(k1,k2)<= numMutations) && countWordWithMutations(sequences,k2,numMutations)))
    yield k2

    return result.toList.distinct
  }

  def medianString(sequences : List[String], k : Int) : String = {
    val patterns = generate(k)
    var bestPattern = patterns.sorted.head
    for (pattern <- patterns) {
      if( kmerHamming(pattern,sequences) <= kmerHamming(bestPattern,sequences)) {
        bestPattern = pattern
      }
    }

    return bestPattern
  }

  def kmerHamming(kmer:String, sequence:String) : Int =  {

    val hammings = for(fragment <- sequence.sliding(kmer.size))
    yield Hamming.calculate(fragment,kmer)

    return hammings.min

  }

  def kmerHamming(kmer:String, sequences:List[String]) : Int =  {

    val hammings = for(sequence <- sequences)
    yield kmerHamming(kmer,sequence)
    return hammings.sum
  }

  def score (sequence : String, matrix : Array[Array[Double]]) : Double = {
    var score : Double = 1
    val splitted = sequence.toCharArray.toList
    for(l <- List.range(0,splitted.size)) {
      val base = splitted(l)
      val col = bases.indexOf(base)
      val line = matrix(l)
      score *= line(col)
    }

    return score
  }

  def findMostProbable(sequence : String, matrix : Array[Array[Double]]) : List[String] = {
    val sequences = sequence.sliding(matrix.size).toList;
    val sorted = sequences.sortBy(DNAProfiler.score(_,matrix)).reverse

    val l = for (se <- sorted; if(sequence.indexOf(se) != -1))
    yield (se,DNAProfiler.score(se,matrix))

    val maxValue = l.toList.maxBy(_._2)._2
    val ret = for (m <- l.filter(_._2 == maxValue)) yield m._1

    return ret.sortBy(sequence.indexOf(_))
  }

  def formProfile(dna : List[String]) : Array[Array[Double]] = {
    val numOfStrainds:Double = dna.size
    val sizeOfStrainds = dna(0).size

    val re = Array.ofDim[Double](4,sizeOfStrainds)

    val cols = for (i<-List.range(0,sizeOfStrainds)) {
      val extracted = for (d<-dna) yield d.substring(i,i+1)
      val count = countBases(extracted)
      val l = List(count(0)/numOfStrainds, count(1)/numOfStrainds, count(2)/numOfStrainds, count(3)/numOfStrainds)
      for (j <- List.range(0,l.size)) {
        re(j)(i) = l(j)
      }
    }
    return re
  }

  def formProfileWithPseudoCount(dna : List[String]) : Array[Array[Double]] = {
    val numOfStrainds:Double = dna.size
    val sizeOfStrainds = dna(0).size

    val re = Array.ofDim[Double](4,sizeOfStrainds)

    val cols = for (i<-List.range(0,sizeOfStrainds)) {
      val extracted = for (d<-dna) yield d.substring(i,i+1)
      val count = countBases(extracted)
      val l = List(count(0)+1/numOfStrainds+4, count(1)+1/numOfStrainds+4, count(2)+1/numOfStrainds+4, count(3)+1/numOfStrainds+4)
      for (j <- List.range(0,l.size)) {
        re(j)(i) = l(j)
      }
    }
    return re
  }




  def greedyMotifSearchWithPseudoCounts(dna : List[String], k : Int, t : Int) : List[String] = {

    val bestMotifs = Array.ofDim[String](t)
    val arrMotifs = Array.ofDim[String](t)
    for (d<-dna) bestMotifs(dna.indexOf(d)) = d.substring(0,k)

    for (motif1 <- dna(0).sliding(k)) {
      arrMotifs(0) = motif1
      for (i <- List.range(1,t)) {
        val m = for (j <- List.range(0,i)) yield arrMotifs(j)
        val prof = DNAProfiler.formProfileWithPseudoCount(m.toList)

        val foundMotif = DNAProfiler.findMostProbable(dna(i),prof.transpose)
        arrMotifs(i) = foundMotif(0)
      }

      val consArr = DNAProfiler.consensus(arrMotifs.toList)
      val score = {for (m <- arrMotifs) yield Hamming.calculate(m,consArr)}.sum

      val consBest = DNAProfiler.consensus(bestMotifs.toList)
      val bestScore = {for (m <- bestMotifs) yield Hamming.calculate(m,consBest)}.sum

      if (score < bestScore) {
        for (i<-List.range(0,t)) bestMotifs(i) = arrMotifs(i)
      }

    }

    return bestMotifs.toList

  }

  def randomizedSearch(dna : List[String], k : Int, t : Int, limit : Int) : List[String] = {
    val upperLimitForKmer : Int = (dna(0).size - k)
    val bestMotifs = Array.ofDim[String](t)
    val arrMotifs = Array.ofDim[String](t)
    for (d<-dna; i=(Random.nextInt() / upperLimitForKmer)) {
      bestMotifs(dna.indexOf(d)) = d.substring(i,i+k)
      println(i + " = " + d.substring(i,i+k))
    }
//
//    for (motif1 <- dna(0).sliding(k)) {
//      arrMotifs(0) = motif1
//      for (i <- List.range(1,t)) {
//        val m = for (j <- List.range(0,i)) yield arrMotifs(j)
//        val prof = DNAProfiler.formProfileWithPseudoCount(m.toList)
//
//        val foundMotif = DNAProfiler.findMostProbable(dna(i),prof.transpose)
//        arrMotifs(i) = foundMotif(0)
//      }
//
//      val consArr = DNAProfiler.consensus(arrMotifs.toList)
//      val score = {for (m <- arrMotifs) yield Hamming.calculate(m,consArr)}.sum
//
//      val consBest = DNAProfiler.consensus(bestMotifs.toList)
//      val bestScore = {for (m <- bestMotifs) yield Hamming.calculate(m,consBest)}.sum
//
//      if (score < bestScore) {
//        for (i<-List.range(0,t)) bestMotifs(i) = arrMotifs(i)
//      }
//
//    }

    return bestMotifs.toList

  }

  def greedyMotifSearch(dna : List[String], k : Int, t : Int) : List[String] = {

    val bestMotifs = Array.ofDim[String](t)
    val arrMotifs = Array.ofDim[String](t)
    for (d<-dna) bestMotifs(dna.indexOf(d)) = d.substring(0,k)

    for (motif1 <- dna(0).sliding(k)) {
      arrMotifs(0) = motif1
      for (i <- List.range(1,t)) {
        val m = for (j <- List.range(0,i)) yield arrMotifs(j)
        val prof = DNAProfiler.formProfile(m.toList)

        val foundMotif = DNAProfiler.findMostProbable(dna(i),prof.transpose)
        arrMotifs(i) = foundMotif(0)
      }

      val consArr = DNAProfiler.consensus(arrMotifs.toList)
      val score = {for (m <- arrMotifs) yield Hamming.calculate(m,consArr)}.sum

      val consBest = DNAProfiler.consensus(bestMotifs.toList)
      val bestScore = {for (m <- bestMotifs) yield Hamming.calculate(m,consBest)}.sum

      if (score < bestScore) {
        for (i<-List.range(0,t)) bestMotifs(i) = arrMotifs(i)
      }

    }

    return bestMotifs.toList

  }

  def score(motif : (Int,Int,Int,Int)) : Int = {
    val values = List(motif._1,motif._2,motif._3,motif._4)
    val maxValue = values.max
    return (values.sum - maxValue)
  }

  def score(motifs : List[(Int,Int,Int,Int)]) : Int =  {
    val scores = for (row <- motifs)
    yield score(row)

    return scores.sum
  }
}