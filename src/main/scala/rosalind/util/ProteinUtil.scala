package br.mil.eb.ime.rosalind.util

import scala.collection.mutable.ListBuffer

/**
 * Created with IntelliJ IDEA.
 * User: jonas
 * Date: 10/10/12
 * Time: 10:37 AM
 * To change this template use File | Settings | File Templates.
 */
object ProteinUtil {

  val validMasses = List.range(57,200)

  def cut(proteinSequence : String) : List[String] = {
    val lowerStart = proteinSequence.indexOf("M")
    val upperStop = proteinSequence.lastIndexOf("$")

    if (lowerStart<0 || upperStop <0 || lowerStart>upperStop)
      return Nil

    val notJunk = proteinSequence.substring(lowerStart,upperStop)
    val toRead = notJunk.split("\\$").toList
    val subjects = {
      for (s<-toRead if (!s.equals("")))
         yield s
      }.toList

    for (s<-subjects; i<-List.range(0,s.size) if s.substring(i).startsWith("M"))
      yield s.substring(i)
  }

  def intWeight(protein : String) : Int = {
    val weight = {
      for (amino <- protein.toCharArray)
      yield amino.toString match {
        case "G" => 57
        case "A" => 71
        case "S" => 87
        case "P" => 97
        case "V" => 99
        case "T" => 101
        case "C" => 103
        case "I" => 113
        case "L" => 113
        case "N" => 114
        case "D" => 115
        case "K" => 128
        case "Q" => 128
        case "E" => 129
        case "M" => 131
        case "H" => 137
        case "F" => 147
        case "R" => 156
        case "Y" => 163
        case "W" => 186
      }
    }.toList.sum
    return weight
  }

  val aminoacids = List("G", "A", "S", "P", "V", "T", "C", "I", "L", "N", "D", "K", "Q", "E", "M", "H", "F", "R", "Y", "W")
  val distinctaminoacids = List("G", "A", "S", "P", "V", "T", "C", "L", "N", "D", "K", "E", "M", "H", "F", "R", "Y", "W")


  def cyclopeptide(spectrum : List[Int]) : List[List[Int]] = {
    val peptides : ListBuffer[String] = collection.mutable.ListBuffer("")
    val result : ListBuffer[List[Int]] = collection.mutable.ListBuffer()
    val possibleAminoAcids = aminoacids.filter(a => spectrum.contains(intWeight(a)))
    val spec = spectrum.sorted

    peptides appendAll possibleAminoAcids

    while (peptides.nonEmpty) {
      //expand List
      val oldpeptides = peptides.toList
      peptides clear

      // expand & trim
      for (m <- oldpeptides; n <- possibleAminoAcids;  weights = generateWeights(m+n) ::: List(intWeight(m+n)); if (weights.size == weights.intersect(spec).size ) ) {
        peptides += m+n
      }


      println(oldpeptides.size + " " + result.size + " " + peptides.size)

      //foreach peptide
      for(peptide <- peptides) {
        val peptideSpectrum = generateSpectrum(peptide)

        //peptide.spectrum = spectrum?
        if (peptideSpectrum.sorted equals spec.sorted) {
          result += generateWeights(peptide)
          peptides remove peptides.indexOf(peptide)
        }

      }
    }
    return result.toList
  }

  def generateWeights(peptide : String) : List[Int] = {
    val l =  {for (amino <- peptide.split(""); if (aminoacids.contains(amino)))
      yield intWeight(amino) }.toList

    if (l.size != peptide.size)
      println("erro")

    return l
  }

  def generateSpectrum(peptide : String) : List[Int] = {
    val possibleCombinations =  { for (i<-List.range(1, peptide.size); sub <- (peptide+peptide.substring(0,i-1)).sliding(i,1) )
    yield sub }.toList

    val p = for ( m <- possibleCombinations)
    yield m.split("").toList.sorted.mkString("")


    val ws = List(0) ::: { for (possible <- p)
    yield ProteinUtil.intWeight(possible) }.toList ::: List(ProteinUtil.intWeight(peptide))

    return ws.sorted
  }

  def generateSpectrum(peptide : List[Int]) : List[Int] = {
    val possibleCombinations =  for (i<-List.range(1, peptide.size);
                                     sub<-(peptide++peptide.slice(0,i-1)).sliding(i,1) )
                                yield sub

    val ws = for (possible <- possibleCombinations)
             yield possible.sum

    return ws.sorted
  }

  def leaderBoardCyclopeptide(spectrum : List[Int], n : Int) : List[List[Int]] = {
    val leaderboard : ListBuffer[(String,Int)] = collection.mutable.ListBuffer(("",0))
    var leaderPeptide : String = ""
    var leaderScore : Int = 0
    val spec = spectrum.sorted
    val parentMass = spec.last

    while (leaderboard.nonEmpty) {
      val old = leaderboard.toList
      leaderboard clear

      // expand
      for (m <- old.toList ; n <- distinctaminoacids) {
        val pep = (m._1+n,intWeight(m._1+n))
        leaderboard += pep
      }

      // expand and update leader
      for (m <- leaderboard.toList) {
        val w = m._2
        if(w == parentMass) {
          val s = score(m._1,spec)
          if(s > leaderScore) {
            leaderPeptide = m._1
            leaderScore = s
//            println("now leader is " + leaderPeptide)
          }
        } else if (w > parentMass) {
          leaderboard.remove(leaderboard indexOf (m))
//          println("removing " + m._1)
        }
      }

      // cut
      val thoseWhoMadeTheCut = { for (pep <- leaderboard.toList) yield (pep._1,pep._2,score(pep._1,spec))}.toList.sortBy(a => a._3).reverse
      if (thoseWhoMadeTheCut.size > 0) {
        val cut = thoseWhoMadeTheCut.slice(0,n).last._3
        val o = thoseWhoMadeTheCut.takeWhile(_._3 >= cut)

        leaderboard clear

        for (m <- o) {
          val pep = (m._1,m._2)
          leaderboard += pep
        }


        println("cut: " + cut + " leaders: "+ leaderboard.size)

      }
    }

    val result = List(generateWeights(leaderPeptide))

    return result.toList


  }

  def score(peptide : String, spectrum : List[Int]) : Int = {
    val peptideSpectrum = generateSpectrum(peptide)
    return peptideSpectrum.intersect(spectrum).size
  }

}