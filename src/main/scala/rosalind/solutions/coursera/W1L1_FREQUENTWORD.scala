package br.mil.eb.ime.rosalind.solutions.coursera

import br.mil.eb.ime.rosalind.algo.DNAProfiler

/**
 * Created with IntelliJ IDEA.
 * User: jonas
 * Date: 11/13/13
 * Time: 8:18 PM
 * To change this template use File | Settings | File Templates.
 */
object W1L1_FREQUENTWORD extends App {
//  val sequence = "ACGTTGCATGTCGCATGATGCATGAGAGCT"
//  val k = 4
  val k = 11
  val sequence = "TACTGGATACTTGGATACTGGTTAAAGATACTTGGACTAATCCTGCATACTTGGATGCATCATATACTTGGATTAAAGTGCATCATTGCATCATTACTGGTGCATCATTTAAAGATACTTGGAATACTTGGATTAAAGATACTTGGAATACTTGGATGCATCATTACTGGATACTTGGAATACTTGGAATACTTGGATACTGGTTAAAGTGCATCATCTAATCCTGCTACTGGATACTTGGAATACTTGGATGCATCATATACTTGGAATACTTGGACTAATCCTGCTGCATCATTACTGGCTAATCCTGCTGCATCATTACTGGATACTTGGATTAAAGCTAATCCTGCATACTTGGAATACTTGGATACTGGATACTTGGACTAATCCTGCATACTTGGAATACTTGGATGCATCATCTAATCCTGCTGCATCATTACTGGATACTTGGAATACTTGGATTAAAGTGCATCATTTAAAGTTAAAGTACTGGCTAATCCTGCTACTGGCTAATCCTGCCTAATCCTGCTACTGGTTAAAGATACTTGGATGCATCATCTAATCCTGCATACTTGGACTAATCCTGCATACTTGGACTAATCCTGCTTAAAGTGCATCATTACTGGTGCATCATTACTGGTACTGGCTAATCCTGCATACTTGGATACTGGTACTGGTGCATCATTACTGGATACTTGGATGCATCATCTAATCCTGCTGCATCATCTAATCCTGCTACTGGATACTTGGACTAATCCTGCCTAATCCTGCTACTGGATACTTGGAATACTTGGATTAAAGTACTGGCTAATCCTGCATACTTGGACTAATCCTGCTTAAAGTGCATCATTTAAAGTTAAAGTACTGGATACTTGGAATACTTGGAATACTTGGATGCATCATTACTGGTTAAAGTACTGGTTAAAGCTAATCCTGC"
  val count = DNAProfiler.countKMers(k,sequence)
  val max  = count.maxBy(_._2)
  for (kmer <- count; if (kmer._2 == max._2))
    print(kmer._1 + " ")

}