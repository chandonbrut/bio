package rosalind.solutions.coursera

import br.mil.eb.ime.rosalind.algo.DNAProfiler

/**
 * Created with IntelliJ IDEA.
 * User: Jonas
 * Date: 03/12/13
 * Time: 09:26
 * To change this template use File | Settings | File Templates.
 */
/*
Sample Input:
     8 5
     CGCCCCTCTCGGGGGTGTTCAGTAAACGGCCA
     GGGCGAGGTATGTGTAAGTGCCAAGGTGCCAG
     TAGTACCGAGACCGAAAGAAGTATACAGGCGT
     TAGATCAAGTTTCAGGTGCACGTCGGTGAACC
     AATCCACCAGCTCCACGTGCAATGTTGGCCTA

Sample Output:
TCTCGGGG
CCAAGGTG
TACAGGCG
TTCAGGTG
TCCACGTG
 */
object W3L6_RANDOMIZED_MOTIF_SEARCH extends App {

  val dna = "CGCCCCTCTCGGGGGTGTTCAGTAAACGGCCA\nGGGCGAGGTATGTGTAAGTGCCAAGGTGCCAG\nTAGTACCGAGACCGAAAGAAGTATACAGGCGT\nTAGATCAAGTTTCAGGTGCACGTCGGTGAACC\nAATCCACCAGCTCCACGTGCAATGTTGGCCTA"
  val inDNA = dna.split("\n").toList
  val k = 8
  val t = 5

  val result = DNAProfiler.randomizedSearch(inDNA,k,t,1000).mkString("\n")
  val expected = "TCTCGGGG\nCCAAGGTG\nTACAGGCG\nTTCAGGTG\nTCCACGTG"
  assert(result equals expected)
  println(result)



}
