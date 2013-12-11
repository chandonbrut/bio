package rosalind.util

import scala.collection.mutable

/**
 * Created by Jonas on 09/12/13.
 */
object SetUtil {

  def arrange(k : Int, alphabet : Set[String]) : Set[String] = {
    if (k>1) {
      val possibilities = for (e <- alphabet; tailed<-arrange(k - 1, alphabet))
                          yield e + tailed
      return possibilities
    }
    if (k==1) {
      val possibilities= for (e<-alphabet)
                         yield e
      return possibilities
    }
    throw new RuntimeException("Recursion error")

  }

  def binArrange(k : Int) : List[String] = {
    val big=Math.pow(2,k).toInt

      for (i<-List.range(0,big))
      yield paddedBin(i,k)

  }

  def paddedBin(i : Int, k: Int) : String = {
    var bin=i.toBinaryString.toString
    val left = k - bin.size
    val p = Array.ofDim[String](left)
    for (i<-List.range(0,p.size)) p(i) = "0"
    val padding =p.mkString("")

    return padding + bin
  }


}
