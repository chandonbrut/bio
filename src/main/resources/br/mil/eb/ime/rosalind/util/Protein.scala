package br.mil.eb.ime.rosalind.util

/**
 * Created with IntelliJ IDEA.
 * User: jonas
 * Date: 10/9/12
 * Time: 12:41 PM
 * To change this template use File | Settings | File Templates.
 */
case class Protein(name:String, sequence:String) {

  val weight = {
    for (s<-sequence.toCharArray)
      yield s.toString match {
        case "A" => 71.03711
        case "C" => 103.00919
        case "D" => 115.02694
        case "E" => 129.04259
        case "F" => 147.06841
        case "G" => 57.02146
        case "H" => 137.05891
        case "I" => 113.08406
        case "K" => 128.09496
        case "L" => 113.08406
        case "M" => 131.04049
        case "N" => 114.04293
        case "P" => 97.05276
        case "Q" => 128.05858
        case "R" => 156.10111
        case "S" => 87.03203
        case "T" => 101.04768
        case "V" => 99.06841
        case "W" => 186.07931
        case "Y" => 163.06333
      }
    }.toList.sum
}
