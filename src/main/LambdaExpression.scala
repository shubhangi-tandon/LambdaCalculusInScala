import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

/**
  * Created by shubhi on 5/16/17.
  */
abstract class LE
case class Var(x:Char) extends LE {
  //println("creating Var " , this.toString)
  override def toString = x.toString
}
case class Lambda(x:Var,e:LE) extends LE {
  //println("creating Lambda " , this.toString)
  def apply(x:LE) = Apply(this,x)
  override def toString = "\\" + x + "." + e
}
case class Apply(e1:LE, e2:LE) extends LE {
  //println("creating application " , this.toString)
  override def toString() = "(" + e1.toString + " " + e2.toString + ")"
}
//for parsing
class LambdaParser extends RegexParsers with PackratParsers{

  def lamdaExpression : Parser[LE] =application| variable| abstraction //|"(" > lamdaExpression < ")" ^^ { identity }
  def term:Parser[LE]  = variable| abstraction|application
  def abstraction: Parser[Lambda] = """\\""".r ~> variable ~ "." ~ term ^^ { case i ~ _ ~ e => Lambda(i, e) }
  def variable: Parser[Var] = "[a-zA-Z]".r ^^ {case x:String=>Var(x.charAt(0))}
  def application: Parser[Apply] = term~' '~term ^^ { case e1 ~' '~e2 => Apply(e1, e2) }
  def apply(input: String) = parseAll(lamdaExpression, input)

}
