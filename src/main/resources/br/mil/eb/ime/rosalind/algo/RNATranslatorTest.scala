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
class RNATranslatorTest extends FunSuite {

  test("Translate") {
  val sequence1 =
    "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
  val expected = "MAMAPRTEINSTRING"
    assert(expected == RNATranslator.codeProtein(sequence1))
  }

}
