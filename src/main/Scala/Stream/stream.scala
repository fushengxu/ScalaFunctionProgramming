package Stream

/**
  * Created by xufusheng on 2017/3/13.
  */
trait Stream[+A]{
  def toListRecursive:List[A] =this match{
    case Cons(h,t)=>(h()::t().toListRecursive)
    case _=>List()
  }

 def toList:List[A]={
   @annotation.tailrec
   def go(s:Stream[A],acc:List[A]):List[A]=s match{
     case Cons(h,t)=>go(t(),h()::acc)
     case _=>acc
   }
   go(this,List()).reverse
 }
  def toListFast:List[A]={
    val buf=new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s:Stream[A]):List[A]=s match{
      case Cons(h,t)=>
        buf+=h()
        go(t())
      case _=>buf.toList
    }
    go(this)
  }
  def take(n:Int):Stream[A]=this match{
    case Cons(h,t) if n>1=>Stream.cons(h(),t().take(n-1))
    case Cons(h,_) if n==1=>Stream.cons(h(),Stream.empty)
    case _=>Stream.empty
  }
  @annotation.tailrec
  final def drop(n:Int):Stream[A]=this match{
    case Cons(_,t) if(n>0)=>t().drop(n-1)
    case _=>this
  }
  def takeWhile(f:A=>Boolean):Stream[A]=this match{
    case Cons(h,t) if f(h())=>Stream.cons(h(),t() takeWhile f)
    case _=>Stream.empty
  }
  def foldRight[B](z: =>B)(f: (A, =>B)=>B):B= this match {
    case Cons(h,t)=>f(h(),t().foldRight(z)(f))
    case _=>z
  }
  def exists(p:A=>Boolean):Boolean=
    foldRight(false)((a,b)=>p(a)||b)
  def forAll(p:A=>Boolean):Boolean=
    foldRight(true)((a,b)=>p(a)&&b)
  def takeWhile_1(f:A=>Boolean):Stream[A]=
    foldRight(Stream.empty[A])((h,t)=>
      if (f(h)) Stream.cons(h,t)
      else Stream.empty)
  def headOption:Option[A]=
    foldRight(None:Option[A])((h,_)=>Some(h))
  def map[B](f:A=>B):Stream[B]=
    foldRight(Stream.empty[B])((h,t)=>Stream.cons(f(h),t))
  def filter(f:A=>Boolean):Stream[A]={
    foldRight(Stream.empty[A])((h,t)=>
      if(f(h)) Stream.cons(h,t)
      else t
    )
  }
  def append[B>:A](s: =>Stream[B]):Stream[B]=
    foldRight(s)((h,t)=>Stream.cons(h,t))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h:()=>A,t:()=>Stream[A]) extends Stream[A]
object Stream{
  def cons[A](hd: =>A,tl: =>Stream[A]):Stream[A]={
    lazy val head=hd
    lazy val tail=tl
    Cons(() =>head, ()=>tail)
  }
  def empty[A]:Stream[A]=Empty
  def apply[A](as:A*):Stream[A]=
    if(as.isEmpty) empty
    else cons(as.head,apply(as.tail:_*))

}
object stream {

  def main(args: Array[String]): Unit = {

  }
}
