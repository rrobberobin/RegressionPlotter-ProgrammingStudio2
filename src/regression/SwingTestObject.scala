package regression

import scala.swing._

object GuI{

def main (args:Array[String]): Unit = {
  
val frame = new MainFrame {
  title = "Simple Regression"
  contents = Button("Click me")(println("click!"))
  
  size = new Dimension(500,500)
  centerOnScreen
}

frame.visible = true
}
}




//object Test extends SimpleSwingApplication {
//  
//}


