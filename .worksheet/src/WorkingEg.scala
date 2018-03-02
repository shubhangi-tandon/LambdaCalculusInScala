import main.Var

object WorkingEg {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(50); 
  print ("hi");$skip(11); 
  var x =2;System.out.println("""x  : Int = """ + $show(x ));$skip(12); 
  var y = 3;System.out.println("""y  : Int = """ + $show(y ));$skip(15); 
  print (x +y)}

  //val  z = new Var('t')

}
