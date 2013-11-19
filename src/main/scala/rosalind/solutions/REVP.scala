package br.mil.eb.ime.rosalind.solutions

import br.mil.eb.ime.rosalind.util.DNA

/**
 * Created with IntelliJ IDEA.
 * User: jonasferreira
 * Date: 10/8/12
 * Time: 7:57 PM
 * To change this template use File | Settings | File Templates.
 */
object REVP extends App {

  val dnaSequence = "CGTTCTTTACCGCAAACCGATCGTATATACATTCACCATGTACAATATGGCGATTCTGCGCTCGCCTGACTAGTGTGGGCGGCCTGAAACACCACCTTAAGTCAGGCCCCGCCTCATCCAAAAGTACTCTGGATCGAGTGGGCACACCATGAGCGTGTACCAACAGGCCGTGTCGAAATCTTTTCGACCTACTATGAAGGTCACATTCAACACCCAGACCCTTGATCCCGCACCTGCTATAGGCTACCTCGCAAGAGAGTGTAAATGGAACCCCGAGCGGTTTGTCAAGGGGTGCCAACCCGGCGGAGTCTCCAACCTAGGGCACTGTAATAACGAAGTCATTCGGGCATTCACTGTCGTCACTCGCGATAGAATGCGAACGCTCAATGTATTCATATGTTATCCGTTTATAAGTAGTAACATTTCGATGGGCTGGCACCTGAGTTATTTGCATACCTATGACACTTGAGTTCATATATAACCCTAAGATCCACATCCCGACCAGAAGATCGTGCTAGGACGTTACTGACGGGTTACTGCAAATGGGGCACCCCATCGTCGGGTGTTACACGGCACGCACCCAATAATTGCACGCCGGTCCAGTTTGTCCCATTGGTGACCCTTACGACTCTCCTTTTTACTACGGGCGCTCTTCGTCTAACTCGCCTGCCATTTACATCAGCGCGATATAATCCCGCGGCTCAGAGTGAGAAAGTCTAGACCACGGGTTGGAATAGCGCGGGAGAGATGGGGGGTGTTCACGGGACGAAGAACCGGCTCCACACCGACACTGCAGTGCCTCCGGGCAGCGCCTCGCGCG"

  for (i<-List.range(4,9);
       sequence<-dnaSequence.sliding(i).toList.zipWithIndex
       if new DNA("unnamed",sequence._1).reverseComplement.sequence.equals(sequence._1))
    println(sequence._2+1 + " " + i)
  //if new DNA("unnamed",sequence).reverseComplement.equals(sequence)
}