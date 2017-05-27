package gettingStarted

/**
  * Created by xufusheng on 2017/2/8.
  */
object Curry {
  def main(args: Array[String]) {
    val curryFunction=new CurryFunction


  }
}
class CurryFunction{
  def partial[A,B,C](a:A,f:(A,B)=>C):B=>C={
    b=>f(a,b)
  }
  def curry[A,B,C](f:(A,B)=>C):A=>(B=>C)={
    (a:A)=>b=>f(a,b)
  }
  def uncurry[A,B,C](f:A=>B=>C):(A,B)=>C={
    (a,b)=>f(a)(b)
  }
  def f(a:Int):Int=2*a
  def g(b:Int):Int=3*b
  def compose[A,B,C](f:B=>C,g:A=>B):A=>C={
    a=>f(g(a))
  }
  //def fg(a:Int):Int=compose(f(_),g(a))
}