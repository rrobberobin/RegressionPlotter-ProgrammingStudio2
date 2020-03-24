package Regression

import scala.io.Source

class FileInput (file:String) {
  
  var dataFile = file
  
  def changeFile(otherFile:String) = {
    dataFile = otherFile
  }
  
  val source = Source.fromFile(dataFile)
  var dataArray = Array[Double]()
  
  
  //data.map(_.toDouble)
  
  while(source.hasNext){
    dataArray = dataArray :+  source.next.toDouble  //.getLines.filter(_.nonEmpty).map(_.toInt).toArray //:+ source.next.toDouble //For both x and y values. Maybe only need one because they are inside the same "next"
    //source.next //extra away
  }
  
  //dataArray.foreach(println(_))
  
  def getArray = dataArray
  
  source.close
  

}