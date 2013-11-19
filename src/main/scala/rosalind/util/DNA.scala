package br.mil.eb.ime.rosalind.util

import br.mil.eb.ime.rosalind.algo.{DNAProfiler, DNATranslator}
import scala.collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: jonas
 * Date: 10/9/12
 * Time: 12:41 PM
 * To change this template use File | Settings | File Templates.
 */
case class DNA(name:String, sequence:String) {

  private var adenine : Int = 0
  private var cytosine : Int = 0
  private var guanine : Int = 0
  private var thymine : Int = 0
  private val startCodom = "ATG"

  for (c <- sequence.toCharArray)
  c match {
    case 'A' => adenine=adenine+1
    case 'G' => guanine=guanine+1
    case 'C' => cytosine=cytosine+1
    case 'T' => thymine=thymine+1
    case _ =>
  }

  def gminusc() : Int = return guanine - cytosine;

  def gcContent() : Double = return (100.0*(guanine+cytosine)) / (adenine+cytosine+guanine+thymine)
  def atgcRatio() : Double = return 1.0*(adenine+thymine)/(guanine+cytosine)

  def fits(other: DNA,upperLimit:Int) : Boolean = {

    val thisReverseUpperLimit = this.sequence.size-upperLimit
    val otherReverseUpperLimit = other.sequence.size-upperLimit

    val mySuffix = this.sequence.substring(thisReverseUpperLimit)
    val otherPrefix = other.sequence.substring(0,upperLimit)

    if (mySuffix.equals(otherPrefix)) return true;
    return false;
  }

  def produceRNA() : RNA = {
    val mRNA = {
      for (base <- this.sequence.toCharArray)
        yield base.toString match {
          case "A" => "A"
          case "C" => "C"
          case "G" => "G"
          case "T" => "U"
      }
    }.toList.mkString
    return new RNA(this.name,mRNA)
  }

  def codeProtein : String = {
    // first, we have to get rid of dump DNA
    val dna = this.sequence
    val startCodomIndex = this.sequence.indexOf(this.startCodom)
    val remaining = this.sequence.substring(startCodomIndex)

    val triplets = remaining.sliding(3,3).toList
    val protein = for (dnaBase <- triplets)
    yield DNATranslator.translateCodom(dnaBase)

    return protein.mkString
  }

  def reverseComplement : DNA = {
      val complementDNA = {
        for (base <- this.sequence.reverse.toCharArray)
        yield base.toString match {
          case "A" => "T"
          case "C" => "G"
          case "G" => "C"
          case "T" => "A"
        }
      }.toList.mkString
      return new DNA(this.name,complementDNA)
    }

  def possibleCoders : List[String] = {
    val remaining = this.sequence
    val complementRemaining = this.reverseComplement.sequence

    val offsetByZero = remaining.sliding(3,3).toList.mkString
    val offsetByOne = remaining.substring(1).sliding(3,3).toList.mkString
    val offsetByTwo = remaining.substring(2).sliding(3,3).toList.mkString


    val complementOffsetByZero = complementRemaining.sliding(3,3).toList.mkString

    val complementOffsetByOne = complementRemaining.substring(1).sliding(3,3).toList.mkString
    val complementOffsetByTwo = complementRemaining.substring(2).sliding(3,3).toList.mkString

    return {
        ProteinUtil.cut(DNATranslator.codeProtein(offsetByZero)) :::
        ProteinUtil.cut(DNATranslator.codeProtein(offsetByOne)) :::
        ProteinUtil.cut(DNATranslator.codeProtein(offsetByTwo)) :::
        ProteinUtil.cut(DNATranslator.codeProtein(complementOffsetByZero)) :::
        ProteinUtil.cut(DNATranslator.codeProtein(complementOffsetByOne)) :::
        ProteinUtil.cut(DNATranslator.codeProtein(complementOffsetByTwo))
    }
  }

  def findKmers(k : Int, region : String) : mutable.Map[String,Int] = {
      val kMers = region.sliding(k)
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

  def findClumps(k:Int, l:Int, t:Int) : List[String] = {
    val possibleRegions = sequence.sliding(l)
    val km =for (region <- possibleRegions; kmer <-findKmers(k,region); if (kmer._2 >= t) )
      yield kmer._1
    km.toList.distinct
  }

}
