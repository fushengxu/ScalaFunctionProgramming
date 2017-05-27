package AKKA
import java.util.concurrent._
import language.implicitConversions

/**
  * Created by xufusheng on 2017/3/
  * o14.
  */
object Par{
  type Par[A]=ExecutorService=>Future[A]
  def run[A](s:ExecutorService)(a:Par[A])=a(s)
  def unit[A](a:A):Par[A]=(es:ExecutorService)=>UnitFuture(a)
  private case class UnitFuture[A](get:A) extends Future[A]{
    def isDone=true
    def get(timeout:Long,units:TimeUnit)=get
    def isCancelled=false
    def cancel(evenIfRuning:Boolean):Boolean=false
  }
  def map2[A,B,C](a:Par[A],b:Par[B])(f:(A,B)=>C):Par[C]={
    (es:ExecutorService)=>{
      val af=a(es)
      val bf=b(es)
      UnitFuture(f(af.get,bf.get))
    }
  }
  def fork[A](a: =>Par[A]):Par[A]=
    es=>es.submit(new Callable[A]{
      def call=a(es).get
    })
  def lazyUnit[A](a: =>A):Par[A]=fork(unit(a))
  def asyncF[A,B](f:A=>B):A=>Par[B]=
    a=>lazyUnit(f(a))
  def map[A,B](pa:Par[A])(f:A=>B):Par[B]=
    map2(pa,unit(()))((a,_)=>f(a))
  def sortPar(parList:Par[List[Int]])=map(parList)(_.sorted)
  def sequence_simple[A](l:List[Par[A]]):Par[List[A]]=
    l.foldRight[Par[List[A]]](unit(List()))((h,t)=>map2(h,t)(_::_))
  def sequenceBalanced[A](as:IndexedSeq[Par[A]]):Par[IndexedSeq[A]]=fork{
    if(as.isEmpty) unit(Vector())
    else if(as.isEmpty) unit(Vector())
    else {
      val (l,r)=as.splitAt(as.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }
  def sum():Int={
    100

  }
  def future1(b: =>Int): Int ={
    b
  }
  def maybe2(b:Boolean,i: =>Int): Unit ={
    lazy val j=i
    if(b) j+j else 0
  }
  def maybe22(b: Boolean,i: =>Int)={
    val j=i;
    if(b) j+j else 0
  }
  def main(args: Array[String]): Unit = {
    //def f=sum
    maybe22(true,{println("hjsfjshj");1+41})
    //future1(10)
  }
}
