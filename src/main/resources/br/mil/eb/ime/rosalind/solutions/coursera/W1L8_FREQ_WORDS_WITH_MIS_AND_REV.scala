package br.mil.eb.ime.rosalind.solutions.coursera

import br.mil.eb.ime.rosalind.algo.DNAProfiler

/**
 * Created with IntelliJ IDEA.
 * User: jonas
 * Date: 11/14/13
 * Time: 12:08 AM
 * To change this template use File | Settings | File Templates.

 dataset_8_5.txt
 */

/*
Sample Input:
     ACGTTGCATGTCGCATGATGCATGAGAGCT
     4 1

Sample Output:
     ATGT ACAT
 */

/*
Input
CTTGCCGGCGCCGATTATACGATCGCGGCCGCTTGCCTTCTTTATAATGCATCGGCGCCGCGATCTTGCTATATACGTACGCTTCGCTTGCATCTTGCGCGCATTACGTACTTATCGATTACTTATCTTCGATGCCGGCCGGCATATGCCGCTTTAGCATCGATCGATCGTACTTTACGCGTATAGCCGCTTCGCTTGCCGTACGCGATGCTAGCATATGCTAGCGCTAATTACTTAT
9 3
Output
AGCGCCGCT AGCGGCGCT

 */
object W1L8_FREQ_WORDS_WITH_MIS_AND_REV extends App {

  val sequence = "AAGAGAGAGAGAGGAGGAGGAGGAGAAGCACGCGGAGGAGAACGGAGGAGCGAAACGCGGCACGGCACGGAGCGGCACGGCAGAGGAGGCAGCAGAGAAAAAAAAAGCAGCAAGAGAACGGAGCGAAGAGAGCAGAGGAGAAGCAAGAGGAGCGGAGGCAGCAAAGCAGCACGAAGCAGAGAAGCAGAGGAGGCAAAGCACGGCAAGAGGCAGAGCGCGCGGCAAAGCAAAAAAAGAGGCA"
  val k = 9
  val tolerance = 2
  val count = DNAProfiler.countKMersWithReverseComplement(k,sequence,tolerance)
  val max  = count.maxBy(_._2)
  for (kmer <- count; if (kmer._2 == max._2))
    print(kmer._1 + " ")
  //  val combs = "gact".combinations(2)
  //  println(combs)
  //  for (c<- combs)
  //    println(c.mkString(""))

}
