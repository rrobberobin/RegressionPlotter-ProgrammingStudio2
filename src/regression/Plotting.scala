package regression

import scalafx.application.JFXApp


//object Plotting extends JFXApp {}



//import scalafx.Includes._
//import scalafx.application.JFXApp
//import scalafx.scene.Scene
//import scalafx.scene.paint.Color._
//import scalafx.scene.shape.Rectangle
//
//object HelloStageDemo extends JFXApp {
//  stage = new JFXApp.PrimaryStage {
//    title.value = "Hello Stage"
//    width = 600
//    height = 450
//    scene = new Scene {
//      fill = LightGreen
//      content = new Rectangle {
//        x = 25
//        y = 40
//        width = 100
//        height = 100
//        fill <== when(hover) choose Green otherwise Red
//      }
//    }
//  }
//}


import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.effect.DropShadow
import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color._
import scalafx.scene.paint.{Stops, LinearGradient}
import scalafx.scene.text.Text

//import scalafx._


object ScalaFXHelloWorld extends scalafx.application.JFXApp {

  stage = new PrimaryStage {
    title = "ScalaFX Hello World"
    scene = new Scene {
      fill = Black
      content = new HBox {
        padding = Insets(20)
        children = Seq(
          new Text {
            text = "Hello "
            style = "-fx-font-size: 48pt"
            fill = new LinearGradient(
              endX = 0,
              stops = Stops(PaleGreen, SeaGreen))
          },
          new Text {
            text = "World!!!"
            style = "-fx-font-size: 48pt"
            fill = new LinearGradient(
              endX = 0,
              stops = Stops(Cyan, DodgerBlue)
            )
            effect = new DropShadow {
              color = DodgerBlue
              radius = 25
              spread = 0.25
            }
          }
        )
      }
    }
  }
}
