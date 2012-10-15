package br.mil.eb.ime.rosalind.solutions

import io.Source
import scala.collection
import collection.mutable
import br.mil.eb.ime.rosalind.util.REARReader

/**
 * Created with IntelliJ IDEA.
 * User: jonasferreira
 * Date: 10/8/12
 * Time: 7:57 PM
 * To change this template use File | Settings | File Templates.
 */
object REAR extends App {

  class Pinger extends Thread {

    override def run() = {
      while(i<total) {
        println(i+"/"+total)
        Thread.sleep(10000)
      }
    }
  }

  class CachePopulator extends Thread {

    override def run() = {

      for (perm<-perms) {
        cache(perm._2)=new mutable.HashSet[Int]
        if (perm._2==1079556)
          println(perm._2+":"+cache(perm._2))
      }

      for (perm<-perms; elem<-reversals(perm._1))
      {
        i=i+1
        cache(perm._2).add(perms(elem))
      }
      println("Done loading.")
    }

  }

  val cache:collection.mutable.HashMap[Int,collection.mutable.Set[Int]] = new collection.mutable.HashMap[Int,collection.mutable.Set[Int]]
  val perms=List(1,2,3,4,5,6,7,8,9,10).permutations.zipWithIndex.toMap
//  val perms=List(1,2,3,4,5).permutations.zipWithIndex.toMap

  var i=0
  val total=perms.size*45
  /*
  perms.par.map(p => {
    for (elem <- reversals(p._1)) {
//      if (i%100000==0)
//        println(i+"/"+total)
//      i=i+1
      cache(p._2) = new mutable.HashSet[Int]
      cache(p._2).add(perms(elem))
    }
  })
  */
  (new CachePopulator).start()
  (new Pinger).start()

  while(true) {
    println("Entre com o nome do arquivo: ")
    val fileName = Console.readLine
    println("Nome do arquivo é: " + fileName)

    val readPerms=REARReader.readFromFile(fileName)
    var result:List[Int] = List()
    for (p<-readPerms) {
      val perm1 = p._1
      val perm2 = p._2
      val wentThrough=mutable.Set[Int]()
      try {
        var lookup=List(perms(perm1))

        var iterations=0

        while(!lookup.contains(perms(perm2))) {
          for (elem<-lookup) wentThrough.add(elem)
          iterations=iterations+1
          lookup=for (previousLevelPerm<-lookup;
                      newPerm<-cache(previousLevelPerm)
          ) yield newPerm
          lookup=lookup.filterNot(wentThrough.contains(_))
          println("iteration:"+iterations + " wentThrough:" + wentThrough)
        }
        println("iteration:"+iterations + " wentThrough:" + wentThrough)
        result = result ::: List(iterations)

      } catch {
        case e: Exception => e.printStackTrace()
      }
    }
    println(result.mkString(" "))
  }

  def reversals(perm:List[Int]):List[List[Int]] = {
    val allPerms = for (i<-(2 to perm.size))
                     yield for (sequ<-perm.sliding(i).toList)
                      yield { val sI=perm.indexOfSlice(sequ)
                        val pre=perm.slice(0,sI)
                        val mid=sequ.reverse
                        val pos=perm.slice(sI+i,perm.size)
                        pre ::: mid ::: pos }

    val perms = for (p<-allPerms; l<-p) yield l
    return perms.toList
  }

}