package testForCase

/**
  * Created by xufusheng on 2017/3/16.
  */
object LazyFunction {
  def maybeTwice2(b:Boolean,i: =>Int)={
    lazy val j=i
    if(b) j+j else 0
  }
  def ma1(tag:String,name:String,op:Option[Function[Int,Seq[String]]]): Unit ={
    op match {
      case Some(f)=>f(20)
      case None=>
    }
  }
  def ma(tag:Long,name:String,op:Option[Function[Int,Seq[String]]]): Unit ={
    op match {
      case Some(f)=>f(20)
      case None=>
    }
  }
  def ma(tag:String,name:String,op:Option[Function[Int,Seq[String]]]): Unit ={
    op match {
      case Some(f)=>f(20)
      case None=>
    }
  }

  def main(args: Array[String]): Unit = {
    def fo=(i:Int)=>{(0 to i).map(_+"p")}
    def f1(i:Int)={(0 to i).map(_+"p")}
    LazyFunction.ma(1L,"Deng",Some(f1 _))
//    maybeTwice2(true,{println("hi");1+41})
//    maybeTwice2(true,{println("hi");1+41})

  }

}
