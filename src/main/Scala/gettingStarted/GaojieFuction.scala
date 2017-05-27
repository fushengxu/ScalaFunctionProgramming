package gettingStarted

/**
  * Created by xufusheng on 2017/2/8.
  */
object GaojieFuction {
  def main(args: Array[String]) {
    val gaoTest=new GaoTest

    val value1=gaoTest.findFirst(Array(1,2,3,4),(k:Int)=>k==5)
    val value2=gaoTest.findFirst(Array(1,3,3,4),(k:Int)=>k==4)
    println("%d %d" ,value1,value2)
    val isSorted1=gaoTest.isSorted(Array(1,2,3,5),(value1:Int,value2:Int)=>value1>=value2)
    val isSorted2=gaoTest.isSorted(Array(1,2,3,5),(value1:Int,value2:Int)=>value1<value2)

    println("%d %d" ,isSorted1,isSorted2)
  }

}
class GaoTest{
  def findFirst[A](as:Array[A],p:A=>Boolean):Int={
    @annotation.tailrec
    def loop(n:Int):Int=
      if(n>=as.length-1) -1
      else if(p(as(n))) n
      else  loop(n+1)
    loop(0)
  }

  def isSorted[A](as:Array[A],ordered: (A,A)=>Boolean):Boolean={
    @annotation.tailrec
    def loop(n:Int):Boolean={
      if(n>=as.length-1) true
      else if(ordered(as(n),as(n+1))==false) false
      else loop(n+1)
    }
    loop(0)
  }
}
