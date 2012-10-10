package br.mil.eb.ime.rosalind.algo

/**
 * Created with IntelliJ IDEA.
 * User: jonasferreira
 * Date: 10/8/12
 * Time: 7:25 PM
 * To change this template use File | Settings | File Templates.
 */
object RNATranslator {

  def translateProtein(proteinBase:String) : List[String] =
    proteinBase match {
      case "A" => List("GCA", "GCC", "GCG", "GCU")
      case "C" => List("UGC", "UGU")
      case "D" => List("GAC", "GAU")
      case "E" => List("GAA", "GAG")
      case "F" => List("UUC", "UUU")
      case "G" => List("GGA", "GGC", "GGG", "GGU")
      case "H" => List("CAC", "CAU")
      case "I" => List("AUA", "AUC", "AUU")
      case "K" => List("AAA", "AAG")
      case "L" => List("CUA", "CUC", "CUG", "CUU", "UUA", "UUG")
      case "M" => List("AUG")
      case "N" => List("AAC", "AAU")
      case "P" => List("CCA", "CCC", "CCG", "CCU")
      case "Q" => List("CAA", "CAG")
      case "R" => List("AGA", "AGG", "CGA", "CGC", "CGG", "CGU")
      case "S" => List("AGC", "AGU", "UCA", "UCC", "UCG", "UCU")
      case "T" => List("ACA", "ACC", "ACG", "ACU")
      case "V" => List("GUA", "GUC", "GUG", "GUU")
      case "W" => List("UGG")
      case "Y" => List("UAC", "UAU")
      case _ => List("UAA", "UAG", "UGA") // case "Stop" => List("UAA", "UAG", "UGA")
    }

  def translateCodom(baseTriple:String) : String =
    baseTriple match {
      case "AAA" => "K"
      case "AAC" => "N"
      case "AAG" => "K"
      case "AAU" => "N"
      case "ACA" => "T"
      case "ACC" => "T"
      case "ACG" => "T"
      case "ACU" => "T"
      case "AGA" => "R"
      case "AGC" => "S"
      case "AGG" => "R"
      case "AGU" => "S"
      case "AUA" => "I"
      case "AUC" => "I"
      case "AUG" => "M"
      case "AUU" => "I"
      case "CAA" => "Q"
      case "CAC" => "H"
      case "CAG" => "Q"
      case "CAU" => "H"
      case "CCA" => "P"
      case "CCC" => "P"
      case "CCG" => "P"
      case "CCU" => "P"
      case "CGA" => "R"
      case "CGC" => "R"
      case "CGG" => "R"
      case "CGU" => "R"
      case "CUA" => "L"
      case "CUC" => "L"
      case "CUG" => "L"
      case "CUU" => "L"
      case "GAA" => "E"
      case "GAC" => "D"
      case "GAG" => "E"
      case "GAU" => "D"
      case "GCA" => "A"
      case "GCC" => "A"
      case "GCG" => "A"
      case "GCU" => "A"
      case "GGA" => "G"
      case "GGC" => "G"
      case "GGG" => "G"
      case "GGU" => "G"
      case "GUA" => "V"
      case "GUC" => "V"
      case "GUG" => "V"
      case "GUU" => "V"
      case "UAA" => "#"//"Stop"
      case "UAC" => "Y"
      case "UAG" => "#"//"Stop"
      case "UAU" => "Y"
      case "UCA" => "S"
      case "UCC" => "S"
      case "UCG" => "S"
      case "UCU" => "S"
      case "UGA" => "#"//"Stop"
      case "UGC" => "C"
      case "UGG" => "W"
      case "UGU" => "C"
      case "UUA" => "L"
      case "UUC" => "F"
      case "UUG" => "L"
      case "UUU" => "F"
      case _ => "#"
    }
  def codeProtein(rna:String) : String = {
    val triplets = rna.sliding(3,3).toList
    val protein = for (rnaBase <- triplets)
      yield translateCodom(rnaBase)
    return protein.mkString
  }
}
