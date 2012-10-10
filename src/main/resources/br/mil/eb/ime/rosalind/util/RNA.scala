package br.mil.eb.ime.rosalind.util

/**
 * Created with IntelliJ IDEA.
 * User: jonas
 * Date: 10/9/12
 * Time: 12:41 PM
 * To change this template use File | Settings | File Templates.
 */
case class RNA(name:String, sequence:String) {

  private var adenine : Int = 0
  private var cytosine : Int = 0
  private var guanine : Int = 0
  private var uracil : Int = 0
  private val startCodom = "AUG"

  for (c <- sequence.toCharArray)
  c match {
    case 'A' => adenine=adenine+1
    case 'G' => guanine=guanine+1
    case 'C' => cytosine=cytosine+1
    case 'U' => uracil=uracil+1
    case _ =>
  }

  def gcContent() : Double = return (100.0*(guanine+cytosine)) / (adenine+cytosine+guanine+uracil)
  def augcRatio() : Double = return 1.0*(adenine+uracil)/(guanine+cytosine)

  def fits(other: RNA,upperLimit:Int) : Boolean = {

    val thisReverseUpperLimit = this.sequence.size-upperLimit
    val otherReverseUpperLimit = other.sequence.size-upperLimit

    val mySuffix = this.sequence.substring(thisReverseUpperLimit)
    val otherPrefix = other.sequence.substring(0,upperLimit)

    if (mySuffix.equals(otherPrefix)) return true;
    return false;
  }

  def codeProtein : String = {
    val rna = this.sequence
    val startCodomIndex = this.sequence.indexOf(this.startCodom)
    val remaining = this.sequence.substring(startCodomIndex)
    return remaining
  }

}
