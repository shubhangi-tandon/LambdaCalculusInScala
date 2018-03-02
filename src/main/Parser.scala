package main;

/**
  * Created by shubhi on 6/11/17.
  */
class Parser{
  def parse(): Unit = {
    import scala.sys.process._
    val fpath ="/Users/shubhi/Public/ProgrammingLanguages/parser "
    var contents = (fpath + "a+bc aabc").!!
    println(contents)
  }

}
object Parser{

  def main(args: Array[String]): Unit = {
    val p  = new Parser()
    p.parse()
  }

}