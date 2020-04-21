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

  var inputFile = new FileInput("data3.csv") //If It's empty, throw an error

  var dataPerLine = 3 //More dataPerLine means less lines

  var xAxis = NumberAxis()
  var yAxis = NumberAxis()
  var xAxis2 = NumberAxis()
  var yAxis2 = NumberAxis()
  var xAxis3 = NumberAxis()
  var yAxis3 = NumberAxis()

  def updateAll { //If It's empty, throw an error

    val pairs = inputFile.pairs

    val lineSlope = RegressionMath.slope(pairs.map(_._1), pairs.map(_._2))
    val xIsZeroAt = RegressionMath.yIntersect(pairs.map(_._1), pairs.map(_._2))
    val line = Array((pairs.head._1, xIsZeroAt), (pairs.last._1, xIsZeroAt + lineSlope * pairs.last._1))

    val grouped = pairs.sliding(dataPerLine, dataPerLine - 1).toList // Shouldn't work if dataPerLine is 1 or less
    val allX = grouped.map(g => (g.head._1, g.last._1))

    val groupedSlope = grouped.map(g => RegressionMath.slope(g.map(_._1), g.map(_._2)))
    val groupedZeroAt = grouped.map(g => RegressionMath.yIntersect(g.map(_._1), g.map(_._2)))
    val together = (groupedZeroAt zip groupedSlope) zip allX

    val groupedLines = together.map(g => Array((g._2._1, g._1._1 + g._1._2 * g._2._1), (g._2._2, g._1._1 + g._1._2 * g._2._2)))

    stage = new JFXApp.PrimaryStage {
      title = "Total"
      scene = new Scene(1100, 500) {

        val data = XYChart.Series[Number, Number]("Scatter", ObservableBuffer(pairs.map(each => XYChart.Data[Number, Number](each._1, each._2)): _*))
        val xy = new ScatterChart(xAxis, yAxis, ObservableBuffer(data))

        val data2 = XYChart.Series[Number, Number]("Simple Regression", ObservableBuffer(line.map(each => XYChart.Data[Number, Number](each._1, each._2)): _*))
        val xy2 = new LineChart(xAxis2, yAxis2, ObservableBuffer(data2))

        val data3 = groupedLines.map(g => XYChart.Series[Number, Number]("Segmented Regression", ObservableBuffer(g.map(each => XYChart.Data[Number, Number](each._1, each._2)): _*)))
        val xy3 = new LineChart(xAxis3, yAxis3, ObservableBuffer(data3))

        val menuBar = new MenuBar
        val fileMenu = new Menu("File")

        val label = new Label("File: " + inputFile.file)
        label.layoutX = 50
        label.layoutY = 50
        
        val open = new MenuItem("Open")
        val exit = new MenuItem("Exit")

        val regressionMenu = new Menu("Regression")
        //val changeAxises = new MenuItem("Change Axises")

        val segmentsText = new TextField
        segmentsText.layoutX_=(1000)
        segmentsText.layoutY_=(1000)
        segmentsText.prefWidth= 350
        segmentsText.promptText = "How many datapoints per line. Hit enter to confirm"

        val changeAxisSize = new Menu("Change Axis")

        val ChatterX = new MenuItem("ChatterX")
        val ChatterY = new MenuItem("ChatterY")
        val SimpleX = new MenuItem("SimpleX")
        val SimpleY = new MenuItem("SimpleY")
        val SegmentX = new MenuItem("SegmentX")
        val SegmentY = new MenuItem("SegmentY")

        changeAxisSize.items = List(ChatterX, ChatterY, SimpleX, SimpleY, SegmentX, SegmentY)

        //val currentFile = new MenuItem("Current File")

        fileMenu.items = List(open, exit)
        regressionMenu.items = List(changeAxisSize)

        menuBar.menus = List(fileMenu, regressionMenu)
        
        
        val plotPane = new FlowPane()
        plotPane.children = Iterable(xy2, xy3)
        plotPane.setOrientation(Orientation.Vertical)
        
        val plotPane2 = new FlowPane()
        plotPane2.children = Iterable(xy)
        
        val rootPane = new FlowPane();
//        val scene = new Scene(rootPane)
        
        val menuPane = new FlowPane()
        menuPane.children = Iterable(menuBar, label,segmentsText)
//        menuPane.setOrientation(Orientation.Vertical)

        
        rootPane.getChildren().addAll(menuPane, plotPane2, plotPane)
        
        root=rootPane
        
//        content = List(menuBar, xy, xy2, xy3, label, segmentsText)

        exit.onAction = (event: ActionEvent) => sys.exit(0)

        open.onAction = (event: ActionEvent) => {

          val fileChooser = new FileChooser
          val selectedFile = fileChooser.showOpenDialog(stage)
          if (selectedFile != null) {
            //          val path = selectedFile.getPath//.replaceAll("\\\\", "\\\\\\\\")
            //          val betterPath = path.replaceAll("\\\\", "\\\\\\\\")
            inputFile = new FileInput(selectedFile.getPath)
            label.text = "File: " + selectedFile
            updateAll
          }
        }

        ChatterX.onAction = (event: ActionEvent) => {
          val AxisSize = new TextField
          AxisSize.prefWidth = 550
          AxisSize.promptText = "Change size of X-Axis for Chatter Plot. Write in the form:  UpperBound, lowerBound, tickSize   Hit enter to confirm"

          menuPane.children = Iterable(menuBar, label, segmentsText, AxisSize)

          AxisSize.onAction = (event: ActionEvent) => {
            val text = AxisSize.text.apply.split(",").map(_.toInt)
            if (2 < text.size) {
              xAxis = new NumberAxis(text(0), text(1), text(2))
              updateAll
            }
          }
        }

        ChatterY.onAction = (event: ActionEvent) => {
          val AxisSize = new TextField
          AxisSize.prefWidth = 550
          AxisSize.promptText = "Change size of Y-Axis for ChatterPlot. Write in the form:  UpperBound, lowerBound, tickSize   Hit enter to confirm"

          menuPane.children = Iterable(menuBar, label, segmentsText, AxisSize)

          AxisSize.onAction = (event: ActionEvent) => {
            val text = AxisSize.text.apply.split(",").map(_.toInt)
            if (2 < text.size) {
              yAxis = new NumberAxis(text(0), text(1), text(2))
              updateAll
            }
          }
        }

        SimpleX.onAction = (event: ActionEvent) => {
          val AxisSize = new TextField
          AxisSize.prefWidth = 550
          AxisSize.promptText = "Change size of X-Axis for Simple Regression. Write in the form:  UpperBound, lowerBound, tickSize   Hit enter to confirm"

          menuPane.children = Iterable(menuBar, label, segmentsText, AxisSize)

          AxisSize.onAction = (event: ActionEvent) => {
            val text = AxisSize.text.apply.split(",").map(_.toInt)
            if (2 < text.size) {
              xAxis2 = new NumberAxis(text(0), text(1), text(2))
              updateAll
            }
          }
        }

        SimpleY.onAction = (event: ActionEvent) => {
          val AxisSize = new TextField
          AxisSize.prefWidth = 550
          AxisSize.promptText = "Change size of Y-Axis for Simple Regression. Write in the form:  UpperBound, lowerBound, tickSize   Hit enter to confirm"

          menuPane.children = Iterable(menuBar, label, segmentsText, AxisSize)

          AxisSize.onAction = (event: ActionEvent) => {
            val text = AxisSize.text.apply.split(",").map(_.toInt)
            if (2 < text.size) {
              yAxis2 = new NumberAxis(text(0), text(1), text(2))
              updateAll
            }
          }
        }

        SegmentX.onAction = (event: ActionEvent) => {
          val AxisSize = new TextField
          AxisSize.prefWidth = 550
          AxisSize.promptText = "Change size of X-Axis for Segment Regression. Write in the form:  UpperBound, lowerBound, tickSize   Hit enter to confirm"

          menuPane.children = Iterable(menuBar, label, segmentsText, AxisSize)

          AxisSize.onAction = (event: ActionEvent) => {
            val text = AxisSize.text.apply.split(",").map(_.toInt)
            if (2 < text.size) {
              xAxis3 = new NumberAxis(text(0), text(1), text(2))
              updateAll
            }
          }
        }

        SegmentY.onAction = (event: ActionEvent) => {
          val AxisSize = new TextField
          AxisSize.prefWidth = 550
          AxisSize.promptText = "Change size of Y-Axis for Segment Regression. Write in the form:  UpperBound, lowerBound, tickSize   Hit enter to confirm"

          menuPane.children = Iterable(menuBar, label, segmentsText, AxisSize)

          AxisSize.onAction = (event: ActionEvent) => {
            val text = AxisSize.text.apply.split(",").map(_.toInt)
            if (2 < text.size) {
              yAxis3 = new NumberAxis(text(0), text(1), text(2))
              updateAll
            }
          }
        }

        segmentsText.onAction = (event: ActionEvent) => {
          val text = segmentsText.text.apply.toInt
          if (1 < text && text != dataPerLine) {
            dataPerLine = text // && text<pairs.size   maybe?
            updateAll
          }
        }

      }
    }

    stage.setMaximized(true)
  }

  updateAll
}
