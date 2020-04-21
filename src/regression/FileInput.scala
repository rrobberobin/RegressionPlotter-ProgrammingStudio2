package regression

import scala.io.Source

class FileInput(var file: String) {

  
  val filetype =  {
    if(file.endsWith(".csv")) "csv"      
    else if (file.endsWith(".csv")) "json"
    else "unknown"
  }
  
  
  
  val source = Source.fromFile(file)
  //  val theLines = source.getLines.filter(_.nonEmpty)
  //  var dataArray = Array[Double]()
  //
  //  while(source.hasNext){
  //    dataArray = dataArray :+  theLines.next.toDouble   //For both x and y values. Maybe only need one because they are inside the same "next"
  //    //source.next //extra away
  //  }
  //

  var pairs = Array[(Double, Double)]()

  if (source.hasNext && filetype=="csv" && Plotting.commaOrSemicolon=="comma") {            // can't handle (-565-565) or (4455.345.35)
    val rows = source.getLines.filter(_.nonEmpty).toArray
    val commaSeparated = rows.map(_.split(",")) //.map(_.))//.flatten
    val allWeirdDataAway = commaSeparated.map(_.map(_.split("[^\\d.-]").filter(_.nonEmpty))) //:removes all non numbers   //collect{case v:AnyVal =>v}
    val onlyPairs = allWeirdDataAway.filter(_.size > 1)
    val notEmptyPairs = onlyPairs.map(_.filter(!_.isEmpty))
    val numbers = onlyPairs.map(_.map(_.map(_.toDouble)))
    if (!numbers.isEmpty && !numbers.forall(_.isEmpty) && !numbers.forall(_.forall(_.isEmpty))) {
      pairs = numbers.map(x => (x(0)(0), x(1)(0)))
      pairs= pairs.sortBy(_._1)
    }
  }
  
  
  if (source.hasNext && filetype=="csv" && Plotting.commaOrSemicolon=="semicolon") {            // can't handle (-565-565) or (4455.345.35)
    val rows = source.getLines.filter(_.nonEmpty).toArray
    val commaSeparated = rows.map(_.split(";")) //.map(_.))//.flatten
    val allWeirdDataAway = commaSeparated.map(_.map(_.split("[^\\d.-]").filter(_.nonEmpty))) //:removes all non numbers   //collect{case v:AnyVal =>v}
    val onlyPairs = allWeirdDataAway.filter(_.size > 1)
    val notEmptyPairs = onlyPairs.map(_.filter(!_.isEmpty))
    val numbers = onlyPairs.map(_.map(_.map(_.toDouble)))
    if (!numbers.isEmpty && !numbers.forall(_.isEmpty) && !numbers.forall(_.forall(_.isEmpty))) {
      pairs = numbers.map(x => (x(0)(0), x(1)(0)))
      pairs= pairs.sortBy(_._1)
      
    }
  }  
  
  
  
  
  
  
  

//  if(source.hasNext && filetype=="json"){
//    
//    //"rghrtyuiltkjhgrfed".parse
//    
//    
//  }
  

  
  source.close

}