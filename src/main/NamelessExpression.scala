/**
  * Created by shubhi on 5/19/17.
  */

abstract class NLE
case class NVar(x:Int) extends NLE {
  override def toString = x.toString
  //val xInt = x
}
case class NLambda(e:NLE) extends NLE {
  def apply(x:NLE) = NApply(this,x)
  override def toString = "\\"  + "." + e
}
case class NApply(e1:NLE, e2:NLE) extends NLE {
  override def toString() = "(" + e1.toString + " " + e2.toString + ")"
}


