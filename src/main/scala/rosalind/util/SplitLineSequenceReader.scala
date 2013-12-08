package rosalind.util

import java.io.File

/**
 * Created by Jonas on 07/12/13.
 */
trait SplitLineSequenceReader {
  def read(file : File) : List[KMER] = {
    for (sequence <- scala.io.Source.fromFile(file).getLines().toList) yield new KMER(sequence)
  }
}
