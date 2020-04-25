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

  var inputFile = new FileInput("data3.csv")

  var dataPerLine = if (inputFile.pairs.size / 4 > 1) inputFile.pairs.size/4 else 3 //More dataPerLine means less lines //cant be too many lines or can it?

  //creates the axes for the plots
  var xAxis = NumberAxis()
  var yAxis = NumberAxis()
  var xAxis2 = NumberAxis()
  var yAxis2 = NumberAxis()
  var xAxis3 = NumberAxis()
  var yAxis3 = NumberAxis()

  if (inputFile.label.size > 1) {
    xAxis.label = inputFile.label(0)
    yAxis.label = inputFile.label(1)
    xAxis2.label = inputFile.label(0)
    yAxis2.label = inputFile.label(1)
    xAxis3.label = inputFile.label(0)
    yAxis3.label = inputFile.label(1)
  }

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

  //buttons for changing labels and size of axes
  val changeAxisSize = new MenuItem("Change size of the chosen axis")
  val changeAxisLabel = new MenuItem("Change label of the chosen axis") //could i have pressed the label directly to change??? Same with size??? Mark Lewis List interactions...
  //val movePlot = new MenuItem("Change Labels") //can I move by dragging or???  //deselecting textfield when pressing elsewhere....

  //button for changing how many datapoints per line for the segmented regression plot.
  val changeSegments = new MenuItem("Change segment")

  //A menu where we put some of the previously mentioned buttons and menus. We can have menus inside other menus
  val regressionMenu = new Menu("Regression")
  regressionMenu.items = {
    Array(changeSegments, new SeparatorMenuItem, toggleMenu, new SeparatorMenuItem, changeAxisSize, changeAxisLabel)
  } // seperatorMenuItem separates different items

  //a menu bar where we can put the menus. This will show up as a "bar", instead of freely floating menus.
  val menuBar = new MenuBar
  menuBar.menus = Array(fileMenu, regressionMenu)

  def updateAll { //We can update the window when we make changes

    //the data array
    var pairs = inputFile.pairs
    //var pairs = (0 to 100000).map(x=> (x.toDouble,x.toDouble)).toArray

    if (pairs.size > 1000) pairs = pairs.grouped(pairs.size / 1000).map(_.take(1)).flatten.toArray // if data is too large. Data will be grouped to chunks and some will be ignored.

    // the slope of the Simple Regression line
    val lineSlope = RegressionMath.slope(pairs.map(_._1), pairs.map(_._2))

    //the y intersect of the same line
    val xIsZeroAt = RegressionMath.yIntersect(pairs.map(_._1), pairs.map(_._2))

    //line in string form
    val lineString = RegressionMath.linear(pairs.map(_._1), pairs.map(_._2))

    //The same line in array form.
    //The form of the line is: Array((starting x-coordinate, starting y-coordinate)(ending x-coordinate, ending y-coordinate))
    //Mathematically the line would be infinitely long, but we are restriciting the size according to the used data
    val line = Array((pairs.head._1, xIsZeroAt), (pairs.last._1, xIsZeroAt + lineSlope * pairs.last._1))

    // the Segmented Regression lines. They are grouped together in an array where each positon in the array represent one line.
    val grouped = pairs.sliding(dataPerLine, dataPerLine - 1).toArray // Won't work if dataPerLine is 1 or less

    //change all arrays to list if there appears any problems
    //the x-coordinate of the first and the last datapoint
    val allX = grouped.map(g => (g.head._1, g.last._1))

    // the slope of the Segmented Regression lines
    val groupedSlope = grouped.map(g => RegressionMath.slope(g.map(_._1), g.map(_._2)))

    //the y intersect of the same lines
    val groupedZeroAt = grouped.map(g => RegressionMath.yIntersect(g.map(_._1), g.map(_._2)))

    //the y intersect, slopes and x-coordinates geouped together. This is done to easily trasnform the lines into array form
    val together = (groupedZeroAt zip groupedSlope) zip allX

    //the lines in array form
    val groupedLines = together.map(g => Array((g._2._1, g._1._1 + g._1._2 * g._2._1), (g._2._2, g._1._1 + g._1._2 * g._2._2)))

    //lines in string form
    val groupedLineStrings = grouped.map(g => RegressionMath.linear(g.map(_._1), g.map(_._2)))

    //a window which pops up
    stage = new JFXApp.PrimaryStage {
      title = "Regression" //title of window
      scene = new Scene(1200, 850) { //what is displayed inside window

        //this creates a scatterplot
        val scatterData = XYChart.Series[Number, Number](pairs.size + " datapoints", ObservableBuffer(pairs.map(each => XYChart.Data[Number, Number](each._1, each._2)): _*))
        val scatter = new ScatterChart(xAxis, yAxis, ObservableBuffer(scatterData))
        scatter.layoutX = 10 //these specify the position of the plot
        scatter.layoutY = 55 //these specify the position of the plot
        scatter.title = "Scatter Plot"

        //this creates a simple regression plot
        val simpleRegressionData = XYChart.Series[Number, Number](lineString, ObservableBuffer(line.map(each => XYChart.Data[Number, Number](each._1, each._2)): _*))
        val simpleRegression = new LineChart(xAxis2, yAxis2, ObservableBuffer(simpleRegressionData))
        simpleRegression.layoutX = 500
        simpleRegression.layoutY = 55
        simpleRegression.title = "Simple Regression"

        //this creates a segmented regression plot
        val segmentedRegressionData = groupedLines.map(g => XYChart.Series[Number, Number](ObservableBuffer(g.map(each => XYChart.Data[Number, Number](each._1, each._2)): _*)))
        (groupedLineStrings zip segmentedRegressionData).map(x => x._2.name = x._1) //giving names to the lines
        val segmentedRegression = new LineChart(xAxis3, yAxis3, ObservableBuffer(segmentedRegressionData.toList))
        segmentedRegression.layoutX = 500
        segmentedRegression.layoutY = 465
        segmentedRegression.title = "Segmented Regression with " + segmentedRegressionData.size + " segments"

        //shows messages to help the user which happen
        val messages = new Label("Some messages will appear here. Don't restart your program if you want your changes to stay")
        messages.layoutX = 150
        messages.layoutY = 15

        content = Array(menuBar, currentFile, messages, scatter, simpleRegression, segmentedRegression)

        //For closing the program. Implements the action: (press the exit button => exit the progam)
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
              dataPerLine = if (inputFile.pairs.size / 4 > 1) inputFile.pairs.size/4 else 3
              updateAll
            } else messages.text = { //otherwise reports the error
              newFile.possibleError.get
            }
          }
        }

        //for changing delimiter
        changeDelimiter.onAction = (event: ActionEvent) => {
          messages.text = "Change delimiter after having opened the correct file"
          content.add(textField) //adds texfield to window

          //type in the value in the textfield
          textField.onAction = (event: ActionEvent) => {
            val text = textField.text.apply
            if (text.nonEmpty) {
              delimiter = text
              inputFile = new FileInput(inputFile.file)
              updateAll
            }
          }
        }

        //For changing the size of an axis
        changeAxisSize.onAction = (event: ActionEvent) => {
          if (plotToggle.getSelectedToggle == null) messages.text = "You haven't chosen an axis. Choose one first"
          else {
            messages.text = {
              "Change size of Axis. Write in the form: lowerBound,upperBound,tickSize " +
                "You need to specify all three: E.g. 10,100,10 Hit enter to confirm. Remember to check that you have the correct axis chosen"
            }

            content.add(textField) //adds texfield to window

            //type in the value in the textfield
            textField.onAction = (event: ActionEvent) => {
              val text = textField.text.apply.split(",")
              if (text.size == 3) {
                val res = text.map(_.toDouble)

                if (math.abs(res(1) - res(0)) / 50 > (res(2))) messages.text = "Too small tick size. Try: " + (math.abs(res(1) - res(0)) / 40)
                else {

                  //checking which of the radiobuttons is selected
                  if (ScatterX.isSelected) xAxis = new NumberAxis(res(0), res(1), res(2))
                  else if (ScatterY.isSelected) yAxis = new NumberAxis(res(0), res(1), res(2))
                  else if (SimpleX.isSelected) xAxis2 = new NumberAxis(res(0), res(1), res(2))
                  else if (SimpleY.isSelected) yAxis2 = new NumberAxis(res(0), res(1), res(2))
                  else if (SegmentX.isSelected) xAxis3 = new NumberAxis(res(0), res(1), res(2))
                  else if (SegmentY.isSelected) yAxis3 = new NumberAxis(res(0), res(1), res(2))

                  updateAll
                }
              } else if (text.size != 3) messages.text = {
                "You need to specify all three. Write in the form: lowerBound,upperBound,tickSize. " +
                  "An example of a working solution is: 0,100,10"
              }
            }
          }
        }

        changeAxisLabel.onAction = (event: ActionEvent) => {
          if (plotToggle.getSelectedToggle == null) messages.text = "You haven't chosen an axis. You need to tick one of the axes first"
          else {
            messages.text = "Change label of axis. Hit enter to confirm. Remember to check that you have the correct axis chosen"

            content.add(textField) //adds texfield to window

            //type in the value in the textfield
            textField.onAction = (event: ActionEvent) => {
              val text = textField.text.apply

              //checking which radiobutton is selected
              if (ScatterX.isSelected) xAxis.label = text
              else if (ScatterY.isSelected) yAxis.label = text
              else if (SimpleX.isSelected) xAxis2.label = text
              else if (SimpleY.isSelected) yAxis2.label = text
              else if (SegmentX.isSelected) xAxis3.label = text
              else if (SegmentY.isSelected) yAxis3.label = text

              updateAll
            }
          }
        }

        //We can change how many lines the segment regression consists of
        changeSegments.onAction = (event: ActionEvent) => {
          messages.text = "Change how many datapoints you want per segment. Hit enter to confirm"

          content.add(textField) //adds texfield to window

          //type in the value in the textfield
          textField.onAction = (event: ActionEvent) => {
            val text = textField.text.apply.toInt
            if (1 < text && text != dataPerLine) {
              if (text > inputFile.pairs.size / 11) {
                dataPerLine = text // && text<pairs.size   maybe?
                updateAll
              } else messages.text = "Too few datapoints. Try more than : " + inputFile.pairs.size / 11
            }
          }
        }
      }
    }
    //stage.setMaximized(true)
  }
  updateAll
}
