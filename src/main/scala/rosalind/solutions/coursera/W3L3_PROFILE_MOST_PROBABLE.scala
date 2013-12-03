package rosalind.solutions.coursera

import br.mil.eb.ime.rosalind.algo.DNAProfiler

/**
 * Created with IntelliJ IDEA.
 * User: jonas
 * Date: 12/2/13
 * Time: 12:37 PM
 * To change this template use File | Settings | File Templates.
 */

/*
Sample Input:
ACCTGTTTATTGCCTAAGTTCCGAACAAACCCAATATAGCCCGAGGGCCT
5
A C G T
0.2 0.4 0.3 0.1
0.2 0.3 0.3 0.2
0.3 0.1 0.5 0.1
0.2 0.5 0.2 0.1
0.3 0.1 0.4 0.2

Sample Output:
CCGAG
*/


/*
Input
GGTATGCGCACTTCCGAAGAAGGATGCTCAATCATACAAGACACATTCCATCGAGGTAGTTTGACTGGCGAAGTCCCGACTCGCTCACAACTAGTATCCTGTGAAGTCCAGCGTTGAACGACGTGTTGGCTTTAAGCGCCCTGCTTTTCACCAGTTTCTCTCCTAAGTTCGTTCCAGGTCCAAACTGTGGCACTGCAAAT
7
A C G T
0.357 0.357 0.179 0.107
0.393 0.179 0.143 0.286
0.179 0.179 0.321 0.321
0.214 0.179 0.321 0.286
0.286 0.25 0.25 0.214
0.286 0.25 0.179 0.286
0.393 0.143 0.214 0.25
Output
CATTCCA
 */
object W3L3_PROFILE_MOST_PROBABLE extends App {
  val sequence : String = "TCGACTCATTCGGTAGCGTATGGTCAGCCAAACTTTTGTCTCCAGCACTGTCGCACCGTATGCGATAGTCCGCCGCTACGGGCATACGAACTACGGTCGTGGAGGGATAGGGTACAGTCCCTGCCCTTCAGTGTAGAGACTAACTGAAGCAGGATGAATTGCGACCTGTTCAACGGAGGGTAATCAACCCCCGACACAGC"
  val k : Int = 7
  val probabilities = Array.ofDim[Double](k,4)
//  val matrixContent : String = "0.357 0.357 0.179 0.107\n0.393 0.179 0.143 0.286\n0.179 0.179 0.321 0.321\n0.214 0.179 0.321 0.286\n0.286 0.25 0.25 0.214\n0.286 0.25 0.179 0.286\n0.393 0.143 0.214 0.25"
  val matrixContent = "0.357 0.214 0.107 0.321\n0.214 0.286 0.286 0.214\n0.286 0.107 0.286 0.321\n0.357 0.179 0.286 0.179\n0.107 0.179 0.286 0.429\n0.286 0.286 0.214 0.214\n0.107 0.357 0.214 0.321"
  val content = matrixContent.split("\n").toList


  for (line <- content; i<-List.range(0,4)) {
        val probability = line.split(" ")
        val index = content.indexOf(line)
        val l = probabilities(index)
        l(i) = probability(i).toDouble
        probabilities(index) = l
  }

  val motifs = for (dna <- DNAProfiler.countKMers(k,sequence)) yield dna._1
  println(motifs.size)
  val sorted = motifs.toList.sortBy(DNAProfiler.score(_,probabilities))

  for (s<-sorted)
    println(s + " -> " + DNAProfiler.score(s,probabilities))

}
