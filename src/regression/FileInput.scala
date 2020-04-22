package regression

import scala.io.Source
//import com.fasterxml.jackson._//databind.ObjectMapper
//import com.fasterxml.jackson.module.scala.DefaultScalaModule
//import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper

import scala.io.Codec
import java.nio.charset.CodingErrorAction

//reads the data from a file
class FileInput(var file: String) {

  val filetype = {
    if (file.endsWith(".csv")) "csv"
    else if (file.endsWith(".tsv")) "tsv"
    else if (file.endsWith(".json")) "json" // what about  .txt  ?
    else "unknown"
  }

  //default delimiter is tab
  var delimiter = "	"

  //if data uses the wrong encoding, this will change it
  implicit val codec = Codec("UTF-8")
  codec.onMalformedInput(CodingErrorAction.REPLACE)
  codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

  //the data file
  val source = Source.fromFile(file)

  //pairs will contain the resulting data array when done
  var pairs = Array[(Double, Double)]()

  //labels of the axes
  var label = Array[String]()

  //handles csv and tsv files with comma as delimiter. Has some technical steps
  if (source.hasNext && (filetype == "csv" || filetype == "tsv")) { // can't handle (-565-565) or (4455.345.35) or 55-
    val rows = source.getLines.filter(_.nonEmpty).toArray //turns the data into an Array. Each value in the array is an row in the data file
    if (rows.map(_.contains(";")).reduce(_ || _)) delimiter = ";"
    else if (rows.map(_.contains(",")).reduce(_ || _)) delimiter = ","
    val commaSeparated = rows.map(_.split(delimiter)) //splits the array according to the delimiter
    label = commaSeparated(0)
    val allNonNumbersAway = commaSeparated.map(_.map(_.split("[^\\d.-]").filter(_.nonEmpty))) //removes all non numbers
    val noEmptyMinuses = allNonNumbersAway.map(_.map(_.filter(_.exists(_.isDigit)))) //no minuses without numbers
    var notEmptyPairs = noEmptyMinuses.map(_.filter(!_.isEmpty)) //removes empty rows
    if (delimiter == ";") notEmptyPairs = notEmptyPairs.map(_.drop(2)) //specifically used for the Statfin.csv file
    val onlyPairs = notEmptyPairs.filter(_.size > 1) //removes rows with missing data. The file will crash without this
    //test:  println(onlyPairs.take(5).toVector.map(_.toVector.map(_.toVector)))
    if (!onlyPairs.isEmpty && !onlyPairs.forall(_.isEmpty) && !onlyPairs.forall(_.forall(_.isEmpty))) {
      pairs = onlyPairs.map(x => (x(0)(0).toDouble, x(1)(0).toDouble)) //breaks down the unnecessary Array structure and only chooses the data. Also turns strings into doubles.
      pairs = pairs.sortBy(_._1) //sorts the data by the x coordinate, from small to large
    }
  }
  


  if (source.hasNext && filetype == "json") {

    //val mapper = new ObjectMapper()
    //"rghrtyuiltkjhgrfed".parse

  }

  source.close

}