package regression

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene._
import scalafx.scene.chart.ScatterChart
import scalafx.collections.ObservableBuffer
import scalafx.scene.chart.NumberAxis
import scalafx.scene.chart.XYChart
import scalafx.scene.chart.LineChart
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.event.ActionEvent
import scalafx.Includes._
import scalafx.stage.FileChooser
import scalafx.geometry.Orientation

object Plotting extends JFXApp {

  var delimiter = "," //comma is the default delimiter

  var inputFile = new FileInput("data3.csv") //a demo file is processed //data3.csv

  var dataPerLine = if (inputFile.pairs.size / 4 > 1) inputFile.pairs.size / 4 else 3 //More dataPerLine means less lines //cant be too many lines or can it?

  //creates the axes for the plots
  var xAxis = NumberAxis()
  var yAxis = NumberAxis()
  var xAxis2 = NumberAxis()
  var yAxis2 = NumberAxis()
  var xAxis3 = NumberAxis()
  var yAxis3 = NumberAxis()

  //the labels of the axes
  var scatLabelX = ""
  var scatLabelY = ""
  var simpLabelX = ""
  var simpLabelY = ""
  var segLabelX = ""
  var segLabelY = ""

  //update labels
  if (inputFile.label.size > 1) {
    scatLabelX = inputFile.label(0)
    scatLabelY = inputFile.label(1)
    simpLabelX = inputFile.label(0)
    simpLabelY = inputFile.label(1)
    segLabelX = inputFile.label(0)
    segLabelY = inputFile.label(1)
  }

  //These specify the positions of the plots
  var scatLayX = 10.0
  var scatLayY = 55.0
  var simpLayX = 500.0
  var simpLayY = 55.0
  var segLayX = 500.0
  var segLayY = 465.0

  //the size of the axes
  var scatWidth = 500.0
  var scatHeight = 400.0
  var simpWidth = 500.0
  var simpHeight = 400.0
  var segWidth = 500.0
  var segHeight = 400.0

  //shows current file
  val currentFile = new Label("Current file: " + inputFile.file)
  currentFile.layoutX = 150

  //textField for different uses
  val textField = new TextField
  textField.prefWidth = 350
  textField.layoutX = 150
  textField.layoutY = 35

  val open = new MenuItem("Open") //a button for opening files
  val exit = new MenuItem("Exit") //a button for closing program
  val changeDelimiter = new MenuItem("Change delimiter") //button for changing the delimiter of the file
  val fileMenu = new Menu("File") //menu where we can put buttons or other items
  fileMenu.items = Array(open, new SeparatorMenuItem, changeDelimiter, new SeparatorMenuItem, exit) // buttons added to menu

  //These are so called "radiobuttons". We can toggle the buttons and make use of them for many different purposes.
  //Always if we want to change something for a specific plot, we can just toggle the aforesaid button.
  val ScatterX = new RadioMenuItem("ScatterX")
  val ScatterY = new RadioMenuItem("ScatterY")
  val SimpleX = new RadioMenuItem("SimpleX")
  val SimpleY = new RadioMenuItem("SimpleY")
  val SegmentX = new RadioMenuItem("SegmentX")
  val SegmentY = new RadioMenuItem("SegmentY")

  // //This toggle group makes sure that we can only toggle one radiobutton at a time.
  val plotToggle = new ToggleGroup
  plotToggle.toggles = Array(ScatterX, ScatterY, SimpleX, SimpleY, SegmentX, SegmentY)

  //a menu where the togglebuttons are stored
  val toggleMenu = new Menu("Choose axis to change")
  toggleMenu.items = Array(ScatterX, ScatterY, SimpleX, SimpleY, SegmentX, SegmentY)

  //buttons for changing labels and ranges of axes
  val changeAxisRange = new MenuItem("Change range (and ticksize) of the chosen axis")
  val changeAxisLabel = new MenuItem("Change label of the chosen axis")

  //moving or changing sizes of the plots
  val changeAxisSize = new MenuItem("Change size of the chosen axis")
  val movePlot = new MenuItem("Move a plot")

  //button for changing how many datapoints per line for the segmented regression plot.
  val changeSegments = new MenuItem("Change segments")

  //A menu where we put some of the previously mentioned buttons and menus. We can have menus inside other menus
  val regressionMenu = new Menu("Regression")
  regressionMenu.items = {
    Array(changeSegments, new SeparatorMenuItem, toggleMenu, new SeparatorMenuItem, changeAxisRange, changeAxisLabel, changeAxisSize, movePlot)
  } // seperatorMenuItem separates different items

  //a menu bar where we can put the menus. This will show up as a "bar", instead of freely floating menus.
  val menuBar = new MenuBar
  menuBar.menus = Array(fileMenu, regressionMenu)

  def updateAll { //We can update the window when we make changes

    //the data array
    val pairs = inputFile.pairs

    val restrictedPairs = if (pairs.size > 1000) pairs.grouped(pairs.size / 1000).map(_.take(1)).flatten.toArray else pairs // if data is too large. Data will be grouped to chunks and some will be ignored.

    val dataX = pairs.map(_._1)
    val dataY = pairs.map(_._2)

    // the slope of the Simple Regression line
    val lineSlope = RegressionMath.slope(dataX, dataY)

    //the y intersect of the same line
    val xIsZeroAt = RegressionMath.yIntersect(dataX, dataY)

    //line in string form
    val lineString = RegressionMath.linear(dataX, dataY)

    //The same line in array form.
    //The form of the line is: Array((starting x-coordinate, starting y-coordinate)(ending x-coordinate, ending y-coordinate))
    //Mathematically the line would be infinitely long, but we are restriciting the size according to the used data
    var line = Array((dataX.head, xIsZeroAt), (dataX.last, xIsZeroAt + lineSlope * dataX.last))

    if (lineSlope.isNaN || lineSlope.isInfinite) line = Array() // we don't want to plot a line that has slope NaN or infinite

    // the Segmented Regression lines. They are grouped together in an array where each positon in the array represent one line.
    val groups = restrictedPairs.sliding(dataPerLine, dataPerLine - 1).toArray // Won't work if dataPerLine is 1 or less

    // the slope of the Segmented Regression lines
    val groupedSlope = groups.map(g => RegressionMath.slope(g.map(_._1), g.map(_._2)))

    //we need to remove NaNs and infinites
    val NaNs = groupedSlope.map(x => x.isNaN || x.isInfinite)
    val groupedSlopeWithNoNaNs = (groupedSlope zip NaNs).filterNot(_._2).map(_._1)
    val groupedWithNoNaNs = (groups zip NaNs).filterNot(_._2).map(_._1)

    //the y intersect of the same lines
    val groupedZeroAt = groupedWithNoNaNs.map(g => RegressionMath.yIntersect(g.map(_._1), g.map(_._2)))

    //the x-coordinate of the first and the last datapoint
    val allX = groupedWithNoNaNs.map(g => (g.head._1, g.last._1))

    //the y intersect, slopes and x-coordinates geouped together. This is done to easily trasnform the lines into array form
    val together = (groupedZeroAt zip groupedSlope) zip allX

    //the lines in array form. This is the same formula used as for the simple linear regression line
    var groupedLines = together.map(g => Array((g._2._1, g._1._1 + g._1._2 * g._2._1), (g._2._2, g._1._1 + g._1._2 * g._2._2)))

    //lines in string form
    val groupedLineStrings = groupedWithNoNaNs.map(g => RegressionMath.linear(g.map(_._1), g.map(_._2)))

    //a window which pops up
    stage = new JFXApp.PrimaryStage {
      title = "Regression" //title of window
      scene = new Scene(1200, 850) { //what is displayed inside window

        //this creates a scatterplot
        val scatterData = XYChart.Series[Number, Number](restrictedPairs.size + " datapoints", ObservableBuffer(restrictedPairs.map(each => XYChart.Data[Number, Number](each._1, each._2)): _*))
        val scatter = new ScatterChart(xAxis, yAxis, ObservableBuffer(scatterData))
        scatter.layoutX = scatLayX //position of the plot
        scatter.layoutY = scatLayY
        scatter.title = "Scatter Plot" //title of the plot
        scatter.prefWidth = scatWidth //size of the plot/axes
        scatter.prefHeight = scatHeight
        xAxis.label = scatLabelX //labels of the axis
        yAxis.label = scatLabelY

        //this creates a simple regression plot
        val simpleRegressionData = XYChart.Series[Number, Number](lineString, ObservableBuffer(line.map(each => XYChart.Data[Number, Number](each._1, each._2)): _*))
        val simpleRegression = new LineChart(xAxis2, yAxis2, ObservableBuffer(simpleRegressionData))
        simpleRegression.layoutX = simpLayX //position of the plot
        simpleRegression.layoutY = simpLayY
        simpleRegression.title = "Simple Regression"
        simpleRegression.prefWidth = simpWidth //size of the plot/axes
        simpleRegression.prefHeight = simpHeight
        xAxis2.label = simpLabelX //labels of the axis
        yAxis2.label = simpLabelY

        //this creates a segmented regression plot
        val segmentedRegressionData = groupedLines.map(g => XYChart.Series[Number, Number](ObservableBuffer(g.map(each => XYChart.Data[Number, Number](each._1, each._2)): _*)))
        (groupedLineStrings zip segmentedRegressionData).map(x => x._2.name = x._1) //giving names to the lines
        val segmentedRegression = new LineChart(xAxis3, yAxis3, ObservableBuffer(segmentedRegressionData.toList))
        segmentedRegression.layoutX = segLayX //position of the plot
        segmentedRegression.layoutY = segLayY
        segmentedRegression.title = "Segmented Regression with " + segmentedRegressionData.size + " segments"
        segmentedRegression.prefWidth = segWidth //size of the plot
        segmentedRegression.prefHeight = segHeight
        xAxis3.label = segLabelX //labels of the axis
        yAxis3.label = segLabelY

        //shows messages to help the user which happen
        val messages = new Label("Some messages will appear here. Don't restart your program if you want your changes to stay")
        messages.layoutX = 150
        messages.layoutY = 15

        //insert the content into the window
        content = Array(menuBar, currentFile, messages, scatter, simpleRegression, segmentedRegression)

        //For closing the program. Implements the action: (press the exit button => exit the progam). All these onActions are the functionalities of the buttons
        exit.onAction = (event: ActionEvent) => sys.exit(0)

        //for opening files. Implements the sequence of actions: (press the open button => choose a file => open the file)
        open.onAction = (event: ActionEvent) => {

          val fileChooser = new FileChooser //opens some kind of file explorer
          val selectedFile = fileChooser.showOpenDialog(stage) //chosen file
          if (selectedFile != null) {
            val path = selectedFile.getPath //receives path
            val newFile = new FileInput(path) //creates a file input object
            if (newFile.possibleError == None) { //if everything is all right, updates to the new file
              inputFile = newFile
              currentFile.text = "File: " + selectedFile
              dataPerLine = if (inputFile.pairs.size / 4 > 1) inputFile.pairs.size / 4 else 3 //update dataPerLine

              //update labels
              if (inputFile.label.size > 1) {
                scatLabelX = inputFile.label(0)
                scatLabelY = inputFile.label(1)
                simpLabelX = inputFile.label(0)
                simpLabelY = inputFile.label(1)
                segLabelX = inputFile.label(0)
                segLabelY = inputFile.label(1)
              }

              //reset ranges
              xAxis = NumberAxis()
              yAxis = NumberAxis()
              xAxis2 = NumberAxis()
              yAxis2 = NumberAxis()
              xAxis3 = NumberAxis()
              yAxis3 = NumberAxis()

              updateAll //update window
            } else messages.text = { //otherwise reports the error
              newFile.possibleError.get
            }
          }
        }

        //for changing delimiter
        changeDelimiter.onAction = (event: ActionEvent) => {
          messages.text = "Change delimiter to suit the file you are using"

          //adds texfield to window
          content = Array(menuBar, currentFile, messages, scatter, simpleRegression, segmentedRegression, textField)

          // When you type in a value in the textfield and press enter, the delimiter is updated with typed value.
          textField.onAction = (event: ActionEvent) => {
            val text = textField.text.apply
            if (text.nonEmpty) {
              delimiter = text
              inputFile = new FileInput(inputFile.file) //update file
              updateAll //update file
            }
          }
        }

        //change size of axis
        changeAxisSize.onAction = (event: ActionEvent) => {
          if (plotToggle.getSelectedToggle == null) messages.text = {
            "You haven't chosen an axis. Choose one first. " +
              "Choose one of the axes that is connected to the plot you want to move, both x and y axis suffice"
          }
          else {
            messages.text = {
              "Change size of the selected axis. " +
                "You select a plot by selecting a axis connected to a plot. " +
                "Type a number value as the size. E.g. 500"
            }

            //adds texfield to window
            content = Array(menuBar, currentFile, messages, scatter, simpleRegression, segmentedRegression, textField)

            //type in the value in the textfield
            textField.onAction = (event: ActionEvent) => {
              val text = textField.text.apply

              if (inputFile.parseDouble(text) != None) {

                //checking which of the radiobuttons is selected and update accordingly
                if (ScatterX.isSelected) scatWidth = text.toDouble
                else if (ScatterY.isSelected) scatHeight = text.toDouble
                else if (SimpleX.isSelected) simpWidth = text.toDouble
                else if (SimpleY.isSelected) simpHeight = text.toDouble
                else if (SegmentX.isSelected) segWidth = text.toDouble
                else if (SegmentY.isSelected) segHeight = text.toDouble
                updateAll //update window

              } else if (inputFile.parseDouble(text) == None) messages.text = "The value you wrote is not a number"

            }
          }
        }

        //for moving the plot
        movePlot.onAction = (event: ActionEvent) => {
          if (plotToggle.getSelectedToggle == null) messages.text = "You haven't chosen an axis. Choose one first. " + "Choose one of the axes that is connected to the plot you want to move, both x and y axis suffice"
          else {
            messages.text = "Move the selected plot. Write in the form: x-coordinate,y-coordinate. E.g. 200,100 You need to specify both"

            //adds texfield to window
            content = Array(menuBar, currentFile, messages, scatter, simpleRegression, segmentedRegression, textField)

            //type in the value in the textfield
            textField.onAction = (event: ActionEvent) => {
              val text = textField.text.apply.split(",")

              if (text.size == 2 && !text.exists(inputFile.parseDouble(_) == None)) {
                val res = text.map(_.toDouble)

                //checking which of the radiobuttons is selected and updating values accordingly
                if (ScatterX.isSelected || ScatterY.isSelected) {
                  scatLayX = res(0)
                  scatLayY = res(1)
                } else if (SimpleX.isSelected || SimpleY.isSelected) {
                  simpLayX = res(0)
                  simpLayY = res(1)
                } else if (SegmentX.isSelected || SegmentY.isSelected) {
                  segLayX = res(0)
                  segLayY = res(1)
                }
                updateAll //update window

              } else if (text.size != 2) messages.text = {
                "You need both. Write in the form: x-coordinate,y-coordinate " +
                  "An example of a working solution is: 200,100"
              }
              else if (text.exists(inputFile.parseDouble(_) == None)) messages.text = "Your input contains non-numerical values. All values have to be numbers"

            }
          }
        }

        //For changing the range and ticksize of an axis
        changeAxisRange.onAction = (event: ActionEvent) => {
          if (plotToggle.getSelectedToggle == null) messages.text = "You haven't chosen an axis. Choose one first"
          else {
            messages.text = {
              "Change range and ticksize of Axis. Write in the form: lowerBound,upperBound,tickSize " +
                "You need to specify all three: E.g. 10,100,10 Hit enter to confirm. Remember to check that you have the correct axis chosen"
            }

            //adds texfield to window
            content = Array(menuBar, currentFile, messages, scatter, simpleRegression, segmentedRegression, textField)

            //type in the value in the textfield
            textField.onAction = (event: ActionEvent) => {
              val text = textField.text.apply.split(",")
              if (text.size == 3 && !text.exists(inputFile.parseDouble(_) == None)) {
                val res = text.map(_.toDouble)

                if (math.abs(res(1) - res(0)) / 50 > (res(2))) messages.text = "Too small tick size. Try: " + (math.abs(res(1) - res(0)) / 40)
                else {

                  //checking which of the radiobuttons is selected and updates the axis accordingly
                  if (ScatterX.isSelected) xAxis = new NumberAxis(res(0), res(1), res(2))
                  else if (ScatterY.isSelected) yAxis = new NumberAxis(res(0), res(1), res(2))
                  else if (SimpleX.isSelected) xAxis2 = new NumberAxis(res(0), res(1), res(2))
                  else if (SimpleY.isSelected) yAxis2 = new NumberAxis(res(0), res(1), res(2))
                  else if (SegmentX.isSelected) xAxis3 = new NumberAxis(res(0), res(1), res(2))
                  else if (SegmentY.isSelected) yAxis3 = new NumberAxis(res(0), res(1), res(2))

                  updateAll // update window
                }
              } else if (text.size != 3) messages.text = {
                "You need to specify all three. Write in the form: lowerBound,upperBound,tickSize. " +
                  "An example of a working solution is: 0,100,10"
              }
              else if (text.exists(inputFile.parseDouble(_) == None)) messages.text = "Your input contains non-numerical values. All values have to be numbers"
            }
          }
        }

        //change labels of the axes
        changeAxisLabel.onAction = (event: ActionEvent) => {
          if (plotToggle.getSelectedToggle == null) messages.text = "You haven't chosen an axis. Choose one first"
          else {
            messages.text = "Change label of axis. Hit enter to confirm. Remember to check that you have the correct axis chosen"

            //adds texfield to window
            content = Array(menuBar, currentFile, messages, scatter, simpleRegression, segmentedRegression, textField)

            //type in the value in the textfield
            textField.onAction = (event: ActionEvent) => {
              val text = textField.text.apply

              //checking which radiobutton is selected and updatinh variables accordingly
              if (ScatterX.isSelected) scatLabelX = text
              else if (ScatterY.isSelected) scatLabelY = text
              else if (SimpleX.isSelected) simpLabelX = text
              else if (SimpleY.isSelected) simpLabelY = text
              else if (SegmentX.isSelected) segLabelX = text
              else if (SegmentY.isSelected) segLabelY = text

              updateAll //update window
            }
          }
        }

        //We can change how many lines the segment regression consists of
        changeSegments.onAction = (event: ActionEvent) => {
          messages.text = "Change how many datapoints you want per segment. Hit enter to confirm"

          //adds texfield to window
          content = Array(menuBar, currentFile, messages, scatter, simpleRegression, segmentedRegression, textField)

          //type in the value in the textfield
          textField.onAction = (event: ActionEvent) => {
            val text = textField.text.apply
            if (inputFile.parseInt(text) != None) { //checks that the input can be evaluted as an integer
              if (text.toInt > inputFile.pairs.size / 11) { //the value can't be too small or there would be performance issues
                dataPerLine = text.toInt
                updateAll
              } else messages.text = "Too few datapoints. Try more than : " + inputFile.pairs.size / 11
            } else if (inputFile.parseInt(text) == None) messages.text = "The value you wrote is not an integer"
          }
        }
      }
    }
  }
  updateAll //update window. This only happens once, when the program is started
}
