
//end to end implementation
/*
The test parser object currently does not parse parenthesis .
We are testing it against a list of inputs defined in the array of inputs and printing the final evaluated function
 */
object testParser{
  def parse(input:String):LE={

    val lp = new LambdaParser();
    lp(input) match {
      case lp.Success(matched, _) => {
        println("matched, expression is ", matched)
        return matched.asInstanceOf[LE]

       }

      case lp.Failure(msg, _) => println("FAILURE: " + msg); throw new IllegalArgumentException()
      case lp.Error(msg, _) => println("ERROR: " + msg);throw new IllegalAccessException()
    }
  }
  def main(args: Array[String]): Unit = {
    val interpreter = new Interpreter()

    val x = Var('x')
    val lambdaX = Lambda(x, x)
    val y = Var('y')

    val inputs: Array[String] = Array("\\x.x y", "\\x.x", "x", "\\x.y \\z.x")
    val expressions:Array[LE] = Array(Apply(lambdaX, y ), Apply(lambdaX, x), x, Apply(Lambda(x,y), Lambda(Var('z'), x)))

    val outputs=Array()
    //for ((a,b) <- aList zip bList)
    for ((input, expression) <- inputs zip expressions)
      {
          println()
          println("term is ", input)
          val expression_output = parse(input)

          println(expression_output.toString)
          val final_value =interpreter.parseLambdaExpression(expression);
          println(final_value.toString)
        }
      }




}

