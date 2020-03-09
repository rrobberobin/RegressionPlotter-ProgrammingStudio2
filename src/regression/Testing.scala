package regression

import Regression._    // not sure if needed



//This is a test for the simple linear regression in string form. The method "linear" is used from class Regression
object RegressionTest {
  
  val tester = new Regression
  
  val data = Array((1,2),(2,2),(9,9),(1,1),(3,8),(1,9),(0,0),(2,0),(2,6),(4,9))
  
  val dataDoubles = data.map(x=>(x._1.toDouble,x._2.toDouble))      

  val dataX = dataDoubles.map(_._1)
  
  val dataY = dataDoubles.map(_._2)
  
  //the test is in from of a main method
  def main (args:Array[String]): Unit = {
    println(tester.linear(dataX,dataY))
    
  }
  
}