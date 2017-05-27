package testForCase

/**
  * Created by xufusheng on 2017/2/10.
  */
object TestCaseForMatch {
  def main(args: Array[String]) {

    println(countValue)
    println(countValue)

  }
  def countValue():Double={
    lazy val rice={println("hello!Scala");8.0}
    rice

  }

}
trait A{

}
class B[B](b:B) extends A
object B{
  def unapply(): Unit ={

  }
}
//object A{
//  def count(a:A):A=a match{
//    case B(b)=>a
//    case _=>a
//  }
//}