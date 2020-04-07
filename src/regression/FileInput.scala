package Regression

import scala.io.Source

class FileInput (file:String) {
  
  var dataFile = file
  
  def changeFile(otherFile:String) = {
    dataFile = otherFile
  }
  
  val source = Source.fromFile(dataFile)
  val sourceArray = source.getLines.filter(_.nonEmpty).toArray
//  val theLines = source.getLines.filter(_.nonEmpty)
//  var dataArray = Array[Double]()
//  
//  
//  while(source.hasNext){
//    dataArray = dataArray :+  theLines.next.toDouble   //For both x and y values. Maybe only need one because they are inside the same "next"
//    //source.next //extra away
//  }
//  
//  
//  def getArray = dataArray
  
  val commaSeparated = sourceArray.map(_.split(","))//.map(_.))//.flatten
  val numbers = commaSeparated.map(_.map(_.toDouble))
  val pairs = numbers.map(x => (x(0),x(1)))
  
  source.close
  

}