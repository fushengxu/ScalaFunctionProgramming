package DataStructure

/**
  * Created by xufusheng on 2017/2/9.
  */
sealed trait Listt[+A]
case object Nill extends Listt[Nothing]
case class Conss[+A] (head:A,tail:Listt[A]) extends Listt[A]
object Listt{
  def sum(ints:Listt[Int]):Int=ints match {
    case Nill=>0
    case Conss(x,xs)=>x+sum(xs)
  }
  def product(ds:Listt[Double]):Double=ds match{
    case Nill =>1.0
    case Conss(0.0,_)=>0.0
    case Conss(x,xs)=>x*product(xs)
  }
  def apply[A](as:A*):Listt[A]=
    if(as.isEmpty) Nill
    else Conss(as.head,apply(as.tail:_*))
}
object ListStructure {
  def main(args: Array[String]) {
    val listUsing=new ListtUsing
    val list1=listUsing.dropWhile(Listt(1,2,3,6,2,3),(a:Int)=>a<4)
    println(list1)
  }

}
class ListtUsing{
  def append[A](a:Listt[A],b:Listt[A]):Listt[A]=a match{
    case Nill=>b
    case Conss(h,t)=>Conss(h,append(t,b))
  }
  def tail[A](a:Listt[A]):Listt[A]=a match{
    case Nill=>sys.error("Listt is empty !")
    //case Nill=>a
    case Conss(h,t)=>t
  }
//  def setHead[A](list:Listt[A],h:A):Listt= list match {
//    case Nill=>sys.error("Set head in empty Listt")
//    //case Nill=>Nill
//    case Conss(k,t)=>Conss(h,t)
//  }
  def drop[A](l:Listt[A],n:Int):Listt[A]=
    if(n<=0) l
    else l match {
      case Nill=>Nill
      case Conss(_,t)=>drop(t,n-1)
    }
  def  dropWhile[A](l:Listt[A],f:A=>Boolean):Listt[A]=l match {
    case Conss(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }
  def foldRight[A,B](as:Listt[A],z:B)(f:(A,B)=>B):B=
    as match{
      case Nill=>z
      case Conss(h,t)=>f(h,foldRight(t,z)(f))
    }
  def sum2(ns:Listt[Int])=foldRight(ns,0)((x,y)=>x+y)
  def product2(ns:Listt[Double])=foldRight(ns,1.0)(_*_)
}
