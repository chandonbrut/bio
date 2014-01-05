package rosalind.util

/**
 * Created by Jonas on 25/12/13.
 */
object Levenshtein {

  def editDistance(s1 : String, s2 : String) : Int = {
    val d = Array.ofDim[Int](s1.size+1,s2.size+1)

    for (i<-List.range(1,s1.size+1))
      d(i)(0) = i

    for (j<-List.range(1,s2.size+1))
      d(0)(j) = j

    for (i<-List.range(1,s1.size+1); j <- List.range(1,s2.size+1))
      if (s1(i-1) == s2(j-1)) {
        d(i)(j) = d(i-1)(j-1)
      } else {
        d(i)(j) = List(d(i-1)(j), d(i)(j-1), d(i-1)(j-1)).min + 1
      }

  return d(s1.size)(s2.size)
  }

}
