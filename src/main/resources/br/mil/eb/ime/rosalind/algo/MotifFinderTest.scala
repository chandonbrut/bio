package br.mil.eb.ime.rosalind.algo

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * Created with IntelliJ IDEA.
 * User: jonasferreira
 * Date: 10/8/12
 * Time: 7:02 PM
 * To change this template use File | Settings | File Templates.
 */
@RunWith(classOf[JUnitRunner])
class MotifFinderTest extends FunSuite {

  test("FindMotif") {
  val sequence1 =
    "ACGTACGTACGTACGT"
  val motif="GTA"
  val expected:List[Int] = List(3,7,11)
    assert(expected == MotifFinder.findMotif(motif,sequence1))
  }

}
