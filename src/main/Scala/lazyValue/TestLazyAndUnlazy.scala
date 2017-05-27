package lazyValue


/**
  * Created by xufusheng on 2017/2/28.
  */
sealed trait Listtt[+A]{
//  def headOption[A]: Option[A] = {
//    this match {
//      case Nill => None
//      case Consss(h, t) => Some(h())
//    }
//  }
}
case object Nill extends Listtt[Nothing]
case class Consss[+A] (h: ()=>A,t: ()=>Listtt[A]) extends Listtt[A]
object Listtt{
  def consss[A](hd: =>A,tl: =>Listtt[A]):Listtt[A]= {
    lazy val head=hd
    lazy val tail=tl
    Consss(() =>head,() =>tail)
  }
  def empty[A]:Listtt[A]=Nill
  def apply[A](as:A*):Listtt[A]= {
    if (as.isEmpty) empty
    else consss(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]) {
    //val x=Consss(()=>ex)
  }

}

