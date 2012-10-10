package br.mil.eb.ime.rosalind.solutions

import br.mil.eb.ime.rosalind.algo.{DNATranslator, RNATranslator, DNAProfiler}
import io.Source
import br.mil.eb.ime.rosalind.util.DNA

/**
 * Created with IntelliJ IDEA.
 * User: jonasferreira
 * Date: 10/8/12
 * Time: 7:57 PM
 * To change this template use File | Settings | File Templates.
 */
object ORF extends App {

  val dna = new DNA("unnamed","CGCCACCCCACTTATTGGAGAACACTGTCACGGGGGCTGAGGCGAACGATAGCATTAATATGCATTAGCTTTGCCGGACCTTGGGGTAAAGCCTGTCAGGACGTAGCGACAAAGGTACCCTTTACCTTAGTCTGGGGACCCCAGTAATCGGTGCGCGAAACACTTAAACTGATACTTCCCTTCCATTGGGAAAGAGAACAAGATCGAAACTTTTTTCCGTAAAACGGCAGACCCCGATAACCGCAGAGCGTGGATCCTAAGATAGTAATCTAGGCTACGATATCGCACGTTCCACCTATGTAGAACGGCACCGAAGAGGGTTGTTCCACGAATCCTGGACACATTTCCTTCGCAATGCATACGCCGTCTGGTAGGGGGAAGATCGCAGTACCGCATTCTGATACCCTACTTGAGACCACTTCCGATTCAGCCCTCAATTATGATCTTGGCATTACTATTTAATCGTTAGCTAACGATTAAATAGTAATGCCAAGATCATTTATTACGGCTAGCCCATCACCGAGACCACCTCCTCATACTTGATGGTAAATTATTTAATGCTGAAGACGCAGTATGGAACATATCGCAGACTAGTTCGCACCGATCCTGCCTAGGGCGCGACTGGACCGGTACCGCATTGAGCACGCATTTCGTGTCTCCACGAAGAGGCTATCAGCTAGACGACCATTTTCGACATACTCGTGGTCCCGCCAAAAGGCACGCTCACGCTTACCCCATGAGCTGCTACGGCATCTTGTAGCAGACTATCTGGCAGCCTTGTAGAATACGTTACGTCACCTCACATCAATGCCTGTAGTTGCTGACAATACGTTCTAATGCTATGTAAAATAACGCGGAATCCCTCTCTGTGCACGGAATGGAGTATAAACTAGCAGCCTATATTGCGGATGAGCGGAATTATTAACCGCCATTAGCAA")

  for (prot <- dna.possibleCoders.toSet[String] )
    println(prot)

}
