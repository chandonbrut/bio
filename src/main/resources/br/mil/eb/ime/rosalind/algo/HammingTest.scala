package br.mil.eb.ime.rosalind.algo

import br.mil.eb.ime.rosalind.algo.Hamming
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
class HammingTest extends FunSuite {

  test("Calculate") {
  val sequence1 =
    "GAGCCTACTAACGGGAT"
  val sequence2 =
    "CATCGTAATGACGGCCT"
  val expected = 7
    assert(expected == Hamming.calculate(sequence1,sequence2))
  }

}
