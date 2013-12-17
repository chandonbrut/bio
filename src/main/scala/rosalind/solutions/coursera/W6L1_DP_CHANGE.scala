package rosalind.solutions.coursera

/**
 * Created by jonas on 12/17/13.
 */
object W6L1_DP_CHANGE extends App {


  override def main(args: Array[String]): Unit = {
    println(change(18820,Set(22,20,18,17,10,9,5,3,1)))
  }

  def change(money : Int, coins : Set[Int]) : Int = {

    val minCoins = Array.ofDim[Int](money+1)

    minCoins(0) = 0

    for (m<-List.range(1,money+1)) {
      minCoins(m) = Int.MaxValue
      for (c <- coins.toList) {
        if (m >= c ) {
          if (minCoins(m - c) + 1 < minCoins(m) ) {
            minCoins(m) = minCoins(m - c) + 1
          }
        }
      }
    }

    return minCoins(money)
  }

}
