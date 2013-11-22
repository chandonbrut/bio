package rosalind.util

import br.mil.eb.ime.rosalind.util.ProteinUtil

/**
 * Created with IntelliJ IDEA.
 * User: jonas
 * Date: 11/22/13
 * Time: 12:43 PM
 * To change this template use File | Settings | File Templates.
 */
object ProteinTest extends App {

  val input = "0 71 97 99 103 113 113 114 115 131 137 196 200 202 208 214 226 227 228 240 245 299 311 311 316 327 337 339 340 341 358 408 414 424 429 436 440 442 453 455 471 507 527 537 539 542 551 554 556 566 586 622 638 640 651 653 657 664 669 679 685 735 752 753 754 756 766 777 782 782 794 848 853 865 866 867 879 885 891 893 897 956 962 978 979 980 980 990 994 996 1022 1093".split(" ").toList
  val spectrum = for(i<-input) yield i.toInt

  val a1strings = for (results <- ProteinUtil.cyclopeptide(spectrum).distinct) yield results.mkString("-")
  val a2strings = for (results <- ProteinUtil.massCyclepeptide(spectrum).distinct) yield results.mkString("-")

  println(a1strings.distinct.sorted mkString " ")
  println(a2strings.distinct.sorted mkString " ")
}
