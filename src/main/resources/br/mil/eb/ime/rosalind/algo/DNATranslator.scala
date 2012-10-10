package br.mil.eb.ime.rosalind.algo

/**
 * Created with IntelliJ IDEA.
 * User: jonas
 * Date: 10/10/12
 * Time: 10:37 AM
 * To change this template use File | Settings | File Templates.
 */
object DNATranslator {

  def translateAminoAcid(aminoAcid:String) : List[String] =
    aminoAcid match {
      case "A" => List("GCA", "GCC", "GCG", "GCT")
      case "C" => List("TGC", "TGT")
      case "D" => List("GAC", "GAT")
      case "E" => List("GAA", "GAG")
      case "F" => List("TTC", "TTT")
      case "G" => List("GGA", "GGC", "GGG", "GGT")
      case "H" => List("CAC", "CAT")
      case "I" => List("ATA", "ATC", "ATT")
      case "K" => List("AAA", "AAG")
      case "L" => List("CTA", "CTC", "CTG", "CTT", "TTA", "TTG")
      case "M" => List("ATG")
      case "N" => List("AAC", "AAT")
      case "P" => List("CCA", "CCC", "CCG", "CCT")
      case "Q" => List("CAA", "CAG" )
      case "R" => List("AGA", "AGG", "CGA", "CGC", "CGG", "CGT" )
      case "S" => List("AGC","AGT","TCA","TCC","TCG","TCT")
      case "T" => List("ACA", "ACC", "ACG", "ACT")
      case "V" => List("GTA", "GTC", "GTG", "GTT")
      case "W" => List("TGG")
      case "Y" => List("TAC", "TAT")
      case _ => List("TAA", "TAG", "TGA")
    }

  def translateCodom(codon:String) : String =
    codon match {
      case "TTT" => "F"
      case "CTT" => "L"
      case "ATT" => "I"
      case "GTT" => "V"
      case "TTC" => "F"
      case "CTC" => "L"
      case "ATC" => "I"
      case "GTC" => "V"
      case "TTA" => "L"
      case "CTA" => "L"
      case "ATA" => "I"
      case "GTA" => "V"
      case "TTG" => "L"
      case "CTG" => "L"
      case "ATG" => "M"
      case "GTG" => "V"
      case "TCT" => "S"
      case "CCT" => "P"
      case "ACT" => "T"
      case "GCT" => "A"
      case "TCC" => "S"
      case "CCC" => "P"
      case "ACC" => "T"
      case "GCC" => "A"
      case "TCA" => "S"
      case "CCA" => "P"
      case "ACA" => "T"
      case "GCA" => "A"
      case "TCG" => "S"
      case "CCG" => "P"
      case "ACG" => "T"
      case "GCG" => "A"
      case "TAT" => "Y"
      case "CAT" => "H"
      case "AAT" => "N"
      case "GAT" => "D"
      case "TAC" => "Y"
      case "CAC" => "H"
      case "AAC" => "N"
      case "GAC" => "D"
      case "TAA" => "$" // Stop
      case "CAA" => "Q"
      case "AAA" => "K"
      case "GAA" => "E"
      case "TAG" => "$" // Stop
      case "CAG" => "Q"
      case "AAG" => "K"
      case "GAG" => "E"
      case "TGT" => "C"
      case "CGT" => "R"
      case "AGT" => "S"
      case "GGT" => "G"
      case "TGC" => "C"
      case "CGC" => "R"
      case "AGC" => "S"
      case "GGC" => "G"
      case "TGA" => "$" // Stop
      case "CGA" => "R"
      case "AGA" => "R"
      case "GGA" => "G"
      case "TGG" => "W"
      case "CGG" => "R"
      case "AGG" => "R"
      case "GGG" => "G"
      case _ => "#"
    }

    def codeProtein(sequence:String) : String = {
      val triplets = sequence.sliding(3,3).toList
      val protein = for (dnaBase <- triplets)
      yield translateCodom(dnaBase)
      return protein.mkString
    }


}