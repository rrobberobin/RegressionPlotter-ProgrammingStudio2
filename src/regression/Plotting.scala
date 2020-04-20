package regression


import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene._
import scalafx.scene.chart.ScatterChart
import scalafx.collections.ObservableBuffer
import scalafx.scene.chart.NumberAxis
import scalafx.scene.chart.XYChart
import scalafx.scene.chart.LineChart
import scalafx.scene.layout.FlowPane
import scalafx.scene.control._
import scalafx.event.ActionEvent
import scalafx.Includes._
import scalafx.stage.FileChooser


object Plotting extends JFXApp {
  
//  var inputTesting123 = regression.RegressionTest.inputTest
//  var inputTesting123 = new FileInput("data2.csv")
  
  var inputTesting123 = new FileInput("data3.csv")
  
  def updateAll (newFile:String) {  //If It's empty, throw an error
  
    inputTesting123 = new FileInput(newFile)
    
  val pairs = inputTesting123.pairs
  
  var lineSlope = RegressionMath.slope(pairs.map(_._1),pairs.map(_._2))
  
  var xIsZeroAt = RegressionMath.yIntersect(pairs.map(_._1),pairs.map(_._2))
  
  var line = Array((pairs.head._1,xIsZeroAt),(pairs.last._1,xIsZeroAt+lineSlope*pairs.last._1))
    
  var howManyLines = 2     //can be changed
  
  var pointsPerLine = pairs.size/howManyLines  // shouldn't work if 1 or less
  val grouped = inputTesting123.pairs.sliding(pointsPerLine,pointsPerLine-1).toList
  val allX = grouped.map(g => (g.head._1,g.last._1))
  
  var groupedSlope = grouped.map(g => RegressionMath.slope(g.map(_._1),g.map(_._2)))
  var groupedZeroAt = grouped.map(g => RegressionMath.yIntersect(g.map(_._1),g.map(_._2)))
  
  var together = (groupedZeroAt zip groupedSlope) zip allX
  
  var groupedLines = together.map(g => Array((g._2._1, g._1._1+g._1._2*g._2._1),(g._2._2, g._1._1+g._1._2*g._2._2)))
  
  stage = new JFXApp.PrimaryStage {
    title = "Total"
    scene = new Scene(1100, 500) {
      
      val xAx = NumberAxis()
      val yAx = NumberAxis()
      var data = XYChart.Series[Number, Number]("Scatter", ObservableBuffer(inputTesting123.pairs.map(each => XYChart.Data[Number, Number](each._1,each._2)): _*))
      var xy = new ScatterChart(xAx, yAx, ObservableBuffer(data))
      
      val xAx2 = NumberAxis()
      val yAx2 = NumberAxis()
      var data2 = XYChart.Series[Number, Number]("Simple Regression", ObservableBuffer(line.map(each => XYChart.Data[Number, Number](each._1,each._2)): _*))
      var xy2 = new LineChart(xAx2, yAx2, ObservableBuffer(data2))
      
      val xAx3 = NumberAxis()
      val yAx3 = NumberAxis()
      var data3 = groupedLines.map(g => XYChart.Series[Number, Number]("Segmented Regression",ObservableBuffer(g.map(each => XYChart.Data[Number, Number](each._1,each._2)): _*)))
      var xy3 = new LineChart(xAx3, yAx3, ObservableBuffer(data3))
      
      val menuBar = new MenuBar
      val fileMenu = new Menu("File")
      
      val label = new Label("File: " + inputTesting123.file)
      val open = new MenuItem("Open")
      val exit = new MenuItem("Exit")
      
      //val currentFile = new MenuItem("Current File")
      
      fileMenu.items = List(open,exit)
      
      menuBar.menus = List(fileMenu)
      
      
      exit.onAction = (event:ActionEvent) => sys.exit(0)
      
      open.onAction = (event:ActionEvent) => {
        
        val fileChooser = new FileChooser
        val selectedFile = fileChooser.showOpenDialog(stage)
        if(selectedFile!=null) {   
//          val path = selectedFile.getPath//.replaceAll("\\\\", "\\\\\\\\")
//          val betterPath = path.replaceAll("\\\\", "\\\\\\\\")
          updateAll(selectedFile.getPath)
        label.text = "File: " + selectedFile
        }
      }
      
      var pane = new FlowPane()
      pane.children_=(Iterable(menuBar,xy,xy2,xy3,label))
      root = pane
      
      //content = List(menuBar,xy,xy2, xy3,label)
      
      }
    }
    
  stage.setMaximized(true)
  }
  
  updateAll("data3.csv")
}
