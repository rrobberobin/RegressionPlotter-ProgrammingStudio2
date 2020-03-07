package regression

import scala.math._


class Regression {
  
  
  def dataHandler(data:Array[Double]) = ???
  
  
  //Simple linear regression
  
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
    
    if(data.size<2) correction = 0                             // this correction checker could be unnecessary
    
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
    
  
  def slope (dataX:Array[Double], dataY:Array[Double]) = { 
    
    val numerator = (mean(dataX)-mean(dataY))
    
    val denominator = (mean(dataX)*mean(dataX)-meanOfSquared(dataX))
    
    numerator/denominator
    
  }
  
  def yIntersect (dataX:Array[Double],dataY:Array[Double]) = mean(dataY) - (slope(dataX,dataY)*mean(dataX))
  
  
  def linear (dataX:Array[Double],dataY:Array[Double]):String = {
    
    "y = " + (slope(dataX,dataY)).toString+ "x +" + yIntersect(dataX,dataY)
    
  }
  
  
  
  
  
  
  //Polynomial regression of 2nd degree
  
  
  def polynomial_2nd = ???
  
    
  
}


