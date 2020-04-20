package regression

//import Regression._    // not sure if needed/correct


//This is a test for the simple linear regression in string form. The method "linear" is used from class Regression
object RegressionTest {
  
  
  val data1 = Array((1,2),(2,2),(9,9),(1,1),(3,8),(1,9),(0,0),(2,0),(2,6),(4,9)).map(x=> (x._1.toDouble,x._2.toDouble))
  
  val data2 = Array((1.0,2.0),(2.0,2.5),(3.0,3.0),(9.0,6.0))
  
  val data3 = Array((1.0,2.0),(2.0,2.5),(3.0,3.0))
  
  val data4 = Array((1.0,1.0),(2.0,2.0),(3.0,3.0))
 
  
  def dataDouble (data:Array[(Double,Double)]) = {
    
    val twoBig = data.map(x=>(x._1.toDouble,x._2.toDouble)) 
    
    (twoBig.map(_._1),twoBig.map(_._2))
  }
  
  val inputTest = new FileInput("data.csv")            // same problem here
  val inputTest2 = new FileInput("C:\\Users\\sepie\\git\\project-programming-studio-2-robin-per-l-\\data.csv")
  
  def inputter = inputTest
  
  //The test is in a form of a main method
  def main (args:Array[String]): Unit = {
    println(RegressionMath.linear(dataDouble(data1)._1,dataDouble(data1)._2))
    println(RegressionMath.linear(dataDouble(data2)._1,dataDouble(data2)._2))
    println(RegressionMath.linear(dataDouble(data3)._1,dataDouble(data3)._2))
    println(RegressionMath.linear(dataDouble(data4)._1,dataDouble(data4)._2))
    println(RegressionMath.mean(dataDouble(data4)._2))
    
    println("\nNext test coming up:\n\n")
        
    //inputTest.getArray.foreach(println)    // somehow this returns weird outputs
    //inputTest.numbers.map(_.foreach(println))
    //inputTest.commaSeparated.map(_.mkString).foreach(println)
    val dataX = inputTest.pairs.map(_._1)
    val dataY = inputTest.pairs.map(_._2)
    
    println(RegressionMath.linear(dataX,dataY))
    
    val bestTestYet = new FileInput("data2.csv")
    val deta = bestTestYet.pairs.map(_._1)
    val detaY = bestTestYet.pairs.map(_._2)
    println(RegressionMath.linear(deta,detaY))
  }
  
}