package rosalind.util

/**
 * Created by Jonas on 07/12/13.
 */
case class KMER(val content : String) {

  override def toString() : String = return content

  def mkString() : String = return content

  def overlapsWith(other : KMER) : Boolean = {
    other.prefix() equals suffix()
  }
  def suffix() : KMER = {
    val newContent = content.substring(1,content.size)
    return new KMER(newContent)
  }
  def prefix() : KMER = {
    val newContent = content.substring(0,content.size-1)
    return new KMER(newContent)
  }

  def equals(other : KMER) : Boolean = {
    content equals other.content
  }



}