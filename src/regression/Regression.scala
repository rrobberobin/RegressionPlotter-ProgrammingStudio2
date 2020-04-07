package regression

import scala.math._


/* I have created a lot of methods in this class. 
 * They have been created with extendability in mind and will be more useful when/if I create other regression models. 
 * Many models use the same components.
 */

class SimpleRegression1 {
  
  //def dataHandler(data:Array[Double]) = ???
  
  
/* Here are a lot of methods. No need to go into the specifics of all methods.
 * The method "linear" is the Simple linear regression. It uses the methods "slope" and "yIntersect". 
 * Those methods can be found at the bottom of the page.
 * Some helper methods are used for the implementation of slope and yIntersect.
 * 
 */
  
  def totalSum(data:Array[Double]):Double = {
    
    var total = 0.0
    
    for(each <- data){
      total += each
    }    
    total
  }
  
  def mean (data:Array[Double]): Double = {
    
    totalSum(data)/data.size
  }
  
  def standardDeviation (data:Array[Double]):Double = {
    
    var correction = 1
    
    if(data.size<2) correction = 0        // this correction checker could be unnecessary
    
    var sumOfSquaredDifferences = 0.0
    
    for(each <- data){
      sumOfSquaredDifferences += ((each - mean(data))*(each - mean(data)))
    }
    sqrt(sumOfSquaredDifferences/(data.size-correction))
  }
  
  def covariance (dataX:Array[Double], dataY:Array[Double]) : Double = {
    
    var sumOfDifferences = 0.0
    var xAndY = dataX zip dataY
    
    
    for(each <- xAndY) {
      sumOfDifferences += ((each._1 - mean(dataX))*(each._2 - mean(dataY)))
    }
    sumOfDifferences/xAndY.size
  }
  
  def meanXY (dataX:Array[Double], dataY:Array[Double]) : Double = {
    
    var total = 0.0
    var xAndY = dataX zip dataY
    
    for(each <- xAndY) {
      total += (each._1*each._2)
    }
    total/xAndY.size
  }
  
  def meanOfSquared (data:Array[Double]): Double = mean(data.map(x=> x*x))
    
  
  
  
  //slope formula: slope = (mean(x)*mean(y)-mean(x*y)) / ((mean(x))^2-mean(x^2))
  def slope (dataX:Array[Double], dataY:Array[Double]) = { 
    
    val numerator = (mean(dataX)*mean(dataY)-meanXY(dataX,dataY))
    
    val denominator = (mean(dataX)*mean(dataX)-meanOfSquared(dataX))
    
    numerator/denominator
  }
  
  
  //yIntersect formula: yIntersect = mean(y) - slope*mean(x)
  def yIntersect (dataX:Array[Double],dataY:Array[Double]) = mean(dataY) - (slope(dataX,dataY)*mean(dataX))
  
  
  //Simple linear regression method. It returns a string of the function. (E.g "y=2x+5")
  def linear (dataX:Array[Double],dataY:Array[Double]):String = {
    
    "y = " + (slope(dataX,dataY)).toString+ "x + " + yIntersect(dataX,dataY)
  }
  
  
  
  
  //Polynomial regression of 2nd degree
  
  
  
  
  //def polynomial_2nd = ???
  
    
}


