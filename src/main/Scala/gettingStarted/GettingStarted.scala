package gettingStarted

/**
  * Created by xufusheng on 2017/2/8.
  */
object GettingStarted {
  def main(args: Array[String]) {
    val digui=new DiguiFunction
    val diguiValue=digui.factorical(5)
    println(diguiValue)
    val fibValue=digui.fib(5)
    println(fibValue)
  }

}
class DiguiFunction{
  def factorical(n:Int): Int={
    @annotation.tailrec
    def go(n:Int,acc:Int):Int= {
      if (n <= 0) acc
      else go(n-1,n*acc)

    }
    go(n,1)
  }

  def fib(n:Int):Int={
    @annotation.tailrec
    def go(n:Int,prev:Int,cur:Int):Int={
      if(n==0) prev
      else go(n-1,cur,prev+cur)
    }
    go(n,0,1)
  }
}
