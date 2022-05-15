package regression

import scala.io.Codec
import java.nio.charset.CodingErrorAction

//This is a test for the simple linear regression in string form. The method "linear" is used from class Regression
object RegressionTest {

  val data1 = Array((1, 2), (2, 2), (9, 9), (1, 1), (3, 8), (1, 9), (0, 0), (2, 0), (2, 6), (4, 9)).map(x => (x._1.toDouble, x._2.toDouble))

  val data2 = Array((1.0, 2.0), (2.0, 2.5), (3.0, 3.0), (9.0, 6.0))

  val data3 = Array((1.0, 2.0), (2.0, 2.5), (3.0, 3.0))

  val data4 = Array((1.0, 1.0), (2.0, 2.0), (3.0, 3.0))

  def dataDouble(data: Array[(Double, Double)]) = {

    val twoBig = data.map(x => (x._1.toDouble, x._2.toDouble))

    (twoBig.map(_._1), twoBig.map(_._2))
  }

  Plotting.delimiter = "."

  //test for the fileInput class. Later, I had to change these tests, so that they would still work
  val inputTest = new FileInput("testFiles\\data.csv")
  val inputTest2 = new FileInput("testFiles\\data2.csv")
  val inputTest3 = new FileInput("testFiles\\data3.csv")

  //The testing in form of a main method
  def main(args: Array[String]): Unit = {

    //testing my math methods for different data
    println(RegressionMath.linear(dataDouble(data1)._1, dataDouble(data1)._2))
    println(RegressionMath.linear(dataDouble(data2)._1, dataDouble(data2)._2))
    println(RegressionMath.linear(dataDouble(data3)._1, dataDouble(data3)._2))
    println(RegressionMath.linear(dataDouble(data4)._1, dataDouble(data4)._2))
    println(RegressionMath.mean(dataDouble(data4)._2))

    println("\nNext test coming up:\n\n")

    inputTest.pairs.foreach(println) //testing for different data in fileInput

    val dataX = inputTest.pairs.map(_._1)
    val dataY = inputTest.pairs.map(_._2)

    println(RegressionMath.linear(dataX, dataY))

    val bestTestYet = new FileInput("testFiles\\data2.csv")
    val deta = bestTestYet.pairs.map(_._1)
    val detaY = bestTestYet.pairs.map(_._2)
    println(RegressionMath.linear(deta, detaY))

    println("\nNext test coming up:\n\n")

    implicit val codec = Codec("UTF-8")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

    val a = scala.io.Source.fromFile("testFiles\\Statfin.csv") //testing scala source
    val b = a.getLines
    var n = 0

    while (b.hasNext && n < 15) {

      println(b.next)
      n += 1
    }

    a.close

  }

}