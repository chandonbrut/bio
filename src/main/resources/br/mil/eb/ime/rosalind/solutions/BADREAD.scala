package br.mil.eb.ime.rosalind.solutions

import scala.collection

/**
 * Created with IntelliJ IDEA.
 * User: jonasferreira
 * Date: 10/8/12
 * Time: 7:57 PM
 * To change this template use File | Settings | File Templates.
 */
object BADREAD extends App {

  val ALL_PERMS:Map[List[Int],Int]=List(1,2,3,4,5,6,7,8,9,10).permutations.zipWithIndex.toMap
  val CACHE:collection.mutable.HashMap[Int,List[List[Int]]] = new collection.mutable.HashMap[Int,List[List[Int]]]()

  val PRINCEMEGAHIT: Map[List[Int],List[List[Int]]] = {
    for (p<-List(1,2,3,4,5,6,7,8,9,10).permutations.toList)
    yield (p,reversals(p))}.toMap

  var cacheHit=0
  var cacheMiss=0


  def reversals(perm:List[Int]):List[List[Int]] = {
    if (CACHE.contains(ALL_PERMS(perm))) {
      cacheHit=cacheHit+1

      if (cacheHit%50==0)
        println ("Hit: "+cacheHit + " Miss: " + cacheMiss)

      return CACHE(ALL_PERMS(perm))
    }
    val allPerms = for (i<-(2 to perm.size))
                     yield for (sequ<-perm.sliding(i).toList)
                      yield { val sI=perm.indexOfSlice(sequ)
                        val pre=perm.slice(0,sI)
                        val mid=sequ.reverse
                        val pos=perm.slice(sI+i,perm.size)
  //                      println("("+i+")"+ (pre ::: mid ::: pos))
                        pre ::: mid ::: pos }

    val perms = for (p<-allPerms; l<-p) yield l

    CACHE(ALL_PERMS(perm)) = perms.toList
    cacheMiss=cacheMiss+1

    return CACHE(ALL_PERMS(perm))
  }

  def stringReversals(perm:List[Int]):List[String] = {
    val allPerms = for (i<-(2 to perm.size))
    yield for (sequ<-perm.sliding(i).toList)
      yield { val sI=perm.indexOfSlice(sequ)
        val pre=perm.slice(0,sI)
        val mid=sequ.reverse
        val pos=perm.slice(sI+i,perm.size)
        //                      println("("+i+")"+ (pre ::: mid ::: pos))
        pre ::: mid ::: pos }

    val perms = for (p<-allPerms; l<-p) yield l.mkString

    return perms.toList
  }


//  val MAX_PERMS=List(1,2,3,4,5,6,7,8,9,10).permutations.size
//         println(MAX_PERMS)

  val perm1=List(3,10,8,2,5,4,7,1,6,9)
  val perm2=List(5,2,3,1,7,4,10,8,6,9)
//  val perm1=List(1,2,3,4)
//  val perm2=List(2,1,4,3)
/*
  val lvl1=reversals(perm1).toList
  println(lvl1.size)
  val lvl2=for (lvl<-lvl1) yield reversals(lvl)
  println(lvl2.size)
  val lvl3=for (lvl<-lvl2; l<-lvl.par.map(reversals(_))) yield l
  println(lvl3.size)
  val lvl4=for (lvl<-lvl3; l<-lvl.par.map(reversals(_))) yield l
  println(lvl4.size)
  val lvl5=for (lvl<-lvl4; l<-lvl.par.map(reversals(_))) yield l
  println(lvl5.size)
  */
  /*
  val lvl3=for (lvl<-lvl2) yield reversals(lvl)
  println(lvl3.size)
  val lvl4=lvl3.par.map(reversals(_)).toList
  println(lvl4.size)
  val lvl5=lvl4.par.map(reversals(_)).toList
  println(lvl5.size)*/

  var toLookFor=reversals(perm1)
  var iterations=0
  while(!CACHE.contains(ALL_PERMS(perm2))) {
    iterations=iterations+1
    println("S/H/M:" + CACHE.keys.size + " " + cacheHit + " " + cacheMiss )
    toLookFor=for (previousLevelPerm<-toLookFor;
                   newPerm<-reversals(previousLevelPerm)
                   if !(CACHE.contains(ALL_PERMS(newPerm)))) yield newPerm
    println("iteration:"+iterations)
    }
}