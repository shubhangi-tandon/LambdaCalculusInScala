/**
  * Created by shubhi on 5/19/17.
  */
class Interpreter {

  val context = {'a' to 'z'}
  //free varables, expression
  def removeNames(gamma:List[Char], e:LE):NLE = e match {
      case Var(x) => NVar(gamma.indexOf(x))
      case Lambda(Var(x),e) => NLambda(removeNames(x::gamma, e))
      case Apply(e1,e2) => NApply(removeNames(gamma,e1), removeNames(gamma,e2))
  }

  def getFreeVars(e:LE): List[Char] =e match
  {
    case Var(x)=> x::Nil
    case Lambda(Var(x), e) => getFreeVars(e).filterNot(_ == x)
    case Apply(e1, e2)=>getFreeVars(e1):::getFreeVars(e2)
    case _ => Nil
  }


  //term e, d-place shift, above cutoff c
  def shift(t:NLE, d:Int, c:Int) :NLE = t match {
      case NVar(k) if k<c=> NVar(k)
      case NVar(k) => //println("in shift ",t, d, c, k );
        NVar(k+d)
      case NLambda(t1) => NLambda(shift(t1, d, c+1))
      case NApply(t1, t2) => NApply(shift(t1,d, c), shift(t2, d,c))
  }

  //substitue number j , with term s in term e
  def substitution(j:NLE, s:NLE, e:NLE): NLE=e match {
    case NVar(t1) if (e ==j) => println("in subs e==j ",j,s, e);
      s
    case NVar(t2) =>println("in subs 2 ",j ,s , e); e
    case NLambda(t1)=> println("in subs3 ",j,s, e); NLambda(substitution( shift(j, 1, 0), shift(s, d=1, c=0), t1))
    case NApply(t1, t2)=> println("in sybs4 ",j,s, e); NApply(substitution(j,s,t1), substitution(j,s, t2))
  }

  //define from more specific to generic so e-appabs ,then e-app2 , then e-app1
  def evaluation(e:NLE):NLE =e match {

    case NApply(NLambda(t), NLambda(v))=> println("in apps abs " +  t + NLambda(v))
                              evaluation(shift(
                                substitution(j=NVar(0),
                                  s=shift(NLambda(v), d=1, c=0),
                                  e=t),
                                d= -1, c=0))
    case NApply(NLambda(v), NApply(t1, t2))=> println("in apply v1t2 ", NLambda(v), NApply(t1, t2) ) ;
                              evaluation(NApply(NLambda(v), evaluation(NApply(t1, t2))))
    case NApply(NApply(t1,t2), NApply(t3, t4))=> println("in apply t1t2 ", NApply(t1,t2), NApply(t3, t4) ) ;
                        evaluation(NApply( evaluation(NApply(t1, t2)), NApply(t3, t4)
                                                          ))

    case _ => e
  }

  def restoreNames(e:NLE,domain:List[Char] ):LE = e match {
    case NVar(k)=> Var(domain(k))
    case NLambda(t)=> val index=(context.filterNot(domain.toSet))(0) ;Lambda(x= Var(index), restoreNames(t, index::domain))
    case NApply(t1, t2) => Apply(restoreNames(t1, domain), restoreNames(t2, domain))
  }

  def parseLambdaExpression(term:LE) = {
    println("term is ", term)
    val freeVars = getFreeVars(term)

    println("free vars ", freeVars)
    println ("after remove names", removeNames(freeVars,term ))
    val nle= evaluation(removeNames(freeVars,term ))

    println("nle is ",nle)
    val le = restoreNames( domain = freeVars ,e= nle)
    println("le is ", le)

  }
   //test examples
  def testExamples() : Unit = {

     val c  = Var('c')
     val id = Lambda(c, c)
     val eval =Lambda(c,  Var('f'))
     val term = Apply(id, eval)
     println("for id")
     parseLambdaExpression(term)
     println()

     val a = Var('a')
     val x = Var('x')
     val e0 = Apply(Lambda(x,Apply(x,x)),a)
     parseLambdaExpression(e0)
     println()

     val e1 = Apply(Lambda(x,Apply(x,x)),Lambda( x, a))
     parseLambdaExpression(e1)

     println()
     val lambda1 = Lambda(Var('a'), Var('a'))
     val lambda2 = Lambda(Var('b'), Var('b'))
     val lambda3 = Lambda(Var('c'), Lambda(Var('k'), Apply(Var('c'), Var('k'))))
     val myexpr = Apply(lambda3, Apply(lambda1, lambda2))
     val e2 =  Apply(lambda1, Lambda(x, x))
     println()
     parseLambdaExpression(e2)

     println()
     val expr = Lambda(Var('x'), Apply(Var('y'), Apply(Var('w'), Var('z'))))
     parseLambdaExpression(expr)
     println()
     parseLambdaExpression(myexpr)

   }

}
object Interpreter{
  def main(args: Array[String]): Unit = {
    val c =  new Interpreter()
    c.testExamples()
  }


}
