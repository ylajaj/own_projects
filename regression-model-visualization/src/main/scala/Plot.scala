import UI.{fileMenu, stage, startScene}
import scalafx.scene.Scene
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Rectangle, Shape}
import scalafx.scene.paint.Color.Magenta
import scalafx.scene.paint.Color.DarkMagenta
import scalafx.scene.paint.Color.Blue
import scalafx.scene.paint.Color.DarkBlue
import scalafx.scene.paint.Color._
import scalafx.scene.text.Text
import scalafx.Includes._
import scalafx.event.ActionEvent
import scalafx.scene.control.{Menu, MenuItem}
import scalafx.scene.paint.Color.Cyan
import scalafx.scene.paint.Color.DarkCyan


/**
  * This is the class that provides the method, plot, which creates all the visuals. The user can influence on the
  * appearance of the plot by choosing different parameters, such as constraintX or gridOn.
  */
class Plot(var width: Int, var height: Int, val gridOn: Boolean, val regressionList: Seq[Regression],
           var constraintXBeg: (Double, Double), var constraintYBeg: (Double, Double), var default: Boolean, title: String,
           xlabel: String, ylabel: String, legend: Boolean, legendPos: String, legendtext: Seq[String], dataSize: Int, lineWidth: Int) {

  var coordData = new Data //Class Data provides some useful functionalities.
  var multiplierX = 1.0 // Default order.
  var multiplierY = 1.0 // Default order.
  val mesh = 100.0 // The frequency of the lines plotted

  /**The range of the data sets can be really small or really big (e.q. from 2*10^(-6) to 4*10 ^(-6)). This is a problem since we can only choose
    * our constraints to be integers (when plotting) and big range decreases the speed of the program. We want to modify the x and y data points in a way
    * that the range is at least 10 or at most 1000. This is done by multiplying them with some order of ten (multiplierX and multiplierY below). If the
    * data set has a range of at least of ten or at most 1000, we do not modify it (multiplying with 1.0). If the plot contains many regression models,
    * we multiply all of the data sets with the biggest order of ten.
    */

  if (regressionList.nonEmpty) { //We don't try to change the default order if the regressionList is empty.

    coordData.changeData(regressionList(0).seq) //Changes the data set in coordData.

    /**
      * We create the orders of ten. See method changeMultiplierX and changeMultiplierY in class Data.
      */
    coordData.changeMultiplierX(1.0)
    coordData.changeMultiplierY(1.0)

    multiplierX = coordData.getMultipliers._1 //Order of ten for x.
    multiplierY = coordData.getMultipliers._2 //Order of ten for y.

    /**
      * We check if other regression models have bigger order of magnitude. The order of magnitude for range (1-2)*10^6
      * would be 6 and the multiplierX would be 10^-3. The order of magnitude for range (1-2)*10(-5) would be -5 and the multiplier
      * would be 1000000. (6 zeros)
      */
    for (i <- 1 until regressionList.size) {
      coordData.changeData(regressionList(i).seq)
      coordData.changeMultiplierX(1.0)
      coordData.changeMultiplierY(1.0)
      if (multiplierX > coordData.getMultipliers._1) {
        multiplierX = coordData.getMultipliers._1 // If order of magnitude was bigger, we change the reference order.
      }
      if (multiplierY > coordData.getMultipliers._2) {
        multiplierY = coordData.getMultipliers._2 // If order of magnitude was bigger, we change the reference order.
      }
    }
  }

 /**Changes the constraints of a constraint pair*/
  def changeConstraints(tuple: (Double, Double), value: Double): (Int, Int) = {
    ((tuple._1*value).toInt, (tuple._2*value).toInt)
  }
  /**We change the constraints given by the user. This is done hidden behind the scenes*/
  val constraintX: (Int, Int) = changeConstraints(constraintXBeg, multiplierX)
  val constraintY: (Int, Int) = changeConstraints(constraintYBeg, multiplierY)
  /**
    * The user might want to only plot the data points or the fitted lines. This information is needed for the legend. If the
    * user does not want to plot the data points, he/she sets the dataSize value to zero (lineWidth for fitted lines).
    */
  val plotLines: Int = {
    if (lineWidth == 0) {
      0
    } else
      regressionList.size
  }
  val plotPoints: Int = {
    if (dataSize == 0) {
      0
    } else
      regressionList.size
  }

  /**
    * This method is used to change the range of the data set.
    */
  def multiplyData(seq: Seq[(Double, Double)], x: Double, y: Double): Seq[(Double, Double)] =  {
    val newSeq = seq.map{ case (x1: Double, x2: Double) => (x1*x, x2*y)}
    newSeq
  }

  /**
    * This value is needed to not plot anything inside the legend block.
    */
  val legendtextBiggest: Int = {
    if (legendtext.size  != 0) {
      var biggest = legendtext(0).length
      for (text <- legendtext) {
        if (text.length() > biggest) {
          biggest = text.length
        }
      }
      biggest
    } else
      0
  }
  /**
    * Class CoordinateSystem provides the code for the x and y axis and grid.
    */
  private val coordinateSystem = new CoordinateSystem(width, height, gridOn, constraintX, constraintY,
    (multiplierX,multiplierY), legendtextBiggest, plotLines+plotPoints, legend)

  private val visuals = coordinateSystem.returnVisuals
  private val colorPalet = Seq((Magenta,DarkMagenta),(Blue,DarkBlue),(Red,DarkRed),(Cyan, DarkCyan),(Green,DarkGreen)) // The colors for the plot.

  /**
    * Some constraints to ensure clean legend block.
    */
  val legendConstraints = Seq((coordinateSystem.getPadding +40,coordinateSystem.getPadding),(coordinateSystem.getPadding +85 + legendtextBiggest*8,
    coordinateSystem.getPadding + ((plotLines+plotPoints)-1) * 20 + 20))

  /**
    * Returns true if data point is not inside the legend block.
    */
  def pointWithinLegendConstraints(tuple: (Double, Double)): Boolean = {
    !((tuple._1 > legendConstraints(0)._1 || tuple._1+dataSize > legendConstraints(0)._1)&&(tuple._1 < legendConstraints(1)._1 )
      &&(tuple._2 > legendConstraints(0)._2 || tuple._2+dataSize > legendConstraints(0)._2)&&(tuple._2 < legendConstraints(1)._2))
  }

  var newMenuBar = new scalafx.scene.control.MenuBar //If we want to modify the plot, we return to the starting scene.
  var newMenu = new Menu("File")
  var item = new MenuItem("Back")
  newMenuBar.menus = Seq(newMenu)
  newMenu.items = Seq(item)
  /**
    * Returning to the starting scene.
    */
  item.onAction = (e:ActionEvent ) => {
    for (reg <- regressionList) {
      reg.changeSeq(multiplyData(reg.seq,1/multiplierX,1/multiplierY)) //Changing the regression models into their original form.
    }
    //The size of the stage is always the same in the starting scene.
    stage.width = 730
    stage.height = 370
    stage.setScene(startScene)
    stage.show()
  }

  var buttonSeq = Seq(newMenuBar) //Contents of the Pane are in a sequence.
  
  def getTitle: String = title // Returns the title of the plot.

  /**
    * Text labels.
    */
  var labels = Seq[Text]()
  
  labels = labels :+ new Text(xlabel) {
    x = (width+2*coordinateSystem.getPadding)/2-xlabel.length*4
    y = height+50
  }
  labels = labels :+ new Text(ylabel) {
    rotate = -90
    y = (height+2*coordinateSystem.getPadding)/2-coordinateSystem.getPadding/2 //-ylabel.length*3
    x = coordinateSystem.getPadding-45 -ylabel.length*4

  }

  var legendCurve = Seq[Shape]() // The symbols for the legend
  var legendData = Seq[(Color, String)]() // Legend text labels


  /**
    * We want to map the data set for the appropriate range. For example, if the values of the data points x are in a range of [2,10]
    * and the scene has a width 500, we should map the x values from [2,10] to [0,500]. We obviously want to take the constraints
    * into consideration, but this is done later.
    */
  def mapData(reg: Regression): Seq[(Double, Double)] = {

    val data = new Data
    data.changeData(reg.seq)
    /**
      * Biggest and smallest values of the data set.
      */
    var bigX = data.biggest.get._1
    var bigY = data.biggest.get._2
    var smallX = data.smallest.get._1
    var smallY = data.smallest.get._2
    if (!default) { // If the user sets the constraints. //TODO: Program calculates default constraints.
      smallX = constraintX._1
      bigX = constraintX._2
      smallY = constraintY._1
      bigY = constraintY._2
    }
    reg.seq.map {
      _ match {
        /**
          * If the value is within the constraints we map it to the wanted range. Otherwise it is assigned to a value of (-1.0, -1.0).
          */
        case (x1: Double, x2: Double) if (x1 < constraintX._1 || x1 > constraintX._2 || x2 < constraintY._1 || x2 > constraintY._2) => (-1.0, -1.0)
        case (x1: Double, x2: Double) => ((x1 * width / (bigX - smallX) - width / (bigX - smallX) * smallX +
          coordinateSystem.getPadding).toDouble, (-x2 * (height - coordinateSystem.getPadding) / (bigY - smallY) +
          (height - coordinateSystem.getPadding) / (bigY - smallY) * smallY + height + coordinateSystem.getPadding).toDouble)
      }
    }
  }

  /**
    * Returns the scene including the data points, regression model and coordinate system.
    */
  def plot: Scene = {

    var list: Seq[Shape] = Seq[Shape]() // All the shapes: data points and regression model lines are saved here.
    var counter = 0 // Counter for the color palet.

    for (reg <- this.regressionList) { //We plot all the regression models.
      val data = new Data
      data.changeData(multiplyData(reg.seq, multiplierX, multiplierY))//Change data from none.
      reg.changeSeq(multiplyData(reg.seq,multiplierX,multiplierY))
      val mappedValues: Seq[(Double, Double)] = this.mapData(reg) //The range for the model.
      val loopIncrement: Double = 1/multiplierX
      /**
        * Smallest and biggest values
        */
      var bigX = data.biggest.get._1
      var bigY = data.biggest.get._2
      var smallX = data.smallest.get._1
      var smallY = data.smallest.get._2
      var smallXReal = data.smallest.get._1
      var bigXReal = data.biggest.get._1
      if (!default) { //User sets the constraints.
        smallX = constraintX._1/multiplierX
        bigX = constraintX._2/multiplierX
        smallY = constraintY._1/multiplierY
        bigY = constraintY._2/multiplierY
      }

      /**
        * Linear model
        */
      reg match {
        case Linear(_, _) => {
          /**
            * Plotting the data points
            */
          for (point <- mappedValues) {
            if (point != (-1.0, -1.0) && (pointWithinLegendConstraints((point._1, point._2 - coordinateSystem.getPadding)) || !legend)) {
              list = list :+ new Rectangle() {
                x = point._1 - dataSize / 2.0
                y = point._2 - dataSize / 2.0 - coordinateSystem.getPadding
                width = dataSize
                height = dataSize
                fill = colorPalet(counter)._2
              }
            }
          }
          if (dataSize > 0) {
            legendData = legendData :+ (colorPalet(counter)._2, "square") // Legend symbol
          }
          /**
            * We create small lines which we "clue" together to form a line. We do not create one big line since it could get inside the legend
            * block.
            */
          /**We plot 100 lines between the constraints*/
          val linerLoopIncrement = (bigX-smallX)/mesh
          //val incremmentList = (smallX to bigX by linerLoopIncrement)
          //Changed code
          var incremmentList = Seq[Double]()
          for (i <- 0 to 100) {
              incremmentList = incremmentList :+ (smallX + linerLoopIncrement*i)
          }
          
          //Changed code

          for (increment <- incremmentList) {
            /**
              * y = kx + b
              */
            val y1 = reg.regression().get._2 + reg.regression().get._1 * increment
            val y2 = reg.regression().get._2 + reg.regression().get._1 * (increment + linerLoopIncrement)
            var listX = Seq((increment.toDouble, increment + linerLoopIncrement.toDouble))
            var listY = Seq((y1, y2))
            listX = listX.map {
              _ match {
                case (x1: Double, x2: Double) => ((x1 * width / (bigX - smallX) - width / (bigX - smallX) * smallX + coordinateSystem.getPadding).toDouble,
                  (x2 * width / (bigX - smallX) - width / (bigX - smallX) * smallX + coordinateSystem.getPadding).toDouble)
              }
            }
            listY = listY.map {
              _ match {
                case (x1: Double, x2: Double) => (((-x1 * (height - coordinateSystem.getPadding) / (bigY - smallY) + (height - coordinateSystem.getPadding) / (bigY - smallY) * smallY + height + coordinateSystem.getPadding) - coordinateSystem.getPadding).toDouble,
                  ((-x2 * (height - coordinateSystem.getPadding) / (bigY - smallY) + (height - coordinateSystem.getPadding) / (bigY - smallY) * smallY + height + coordinateSystem.getPadding) - coordinateSystem.getPadding).toDouble)
              }
            }
            //The data point should be inside the coordinate system and outside of the legend block.
            if (((y1 >= smallY && y1 <= bigY) && (y2 >= smallY && y2<= bigY)) && ((increment >= smallX && increment <= bigX) &&
              (increment + linerLoopIncrement >= smallX && increment +linerLoopIncrement <= bigX)) && ((pointWithinLegendConstraints((listX(0)._1, listY(0)._1)) &&
              pointWithinLegendConstraints((listX(0)._2, listY(0)._2))) || !legend)) { //Line is inside the constraints.
              /**
                * Mapping the line into the scene.
                */
              list = list :+ new scalafx.scene.shape.Line() {

                /**
                  * Starting and ending points.
                  */
                startX = listX(0)._1
                endX = listX(0)._2
                startY = listY(0)._1
                endY = listY(0)._2
                strokeWidth = lineWidth
                stroke = colorPalet(counter)._1
              }
            }
          }
          if (lineWidth > 0) {
            legendData = legendData :+ (colorPalet(counter)._1, "line") //Legend symbol
          }
        }
        /**
          * Exponential model
          */
        case Exponential(_, _) => {
          for (point <- mappedValues) { //Plotting points
              if (point != (-1.0, -1.0) && (pointWithinLegendConstraints((point._1, point._2-coordinateSystem.getPadding)) || !legend)) {
                list = list :+ new Rectangle() {
                  x = point._1 - dataSize / 2.0
                  y = point._2 - dataSize / 2.0 - coordinateSystem.getPadding
                  width = dataSize
                  height = dataSize
                  fill = colorPalet(counter)._2
                }
              }
          }
          if (dataSize > 0) {
            legendData = legendData :+ (colorPalet(counter)._2, "square")
          }

          /**We plot 100 lines between the constraints*/
          val expLoopIncrement = (bigX-smallX)/mesh
          //val incrementList = (smallX to bigX by expLoopIncrement)
          //Changed code
          var incrementList = Seq[Double]()
          for (i <- 0 to 100) {
              incrementList = incrementList :+ (smallX + expLoopIncrement*i)
          }
          
          //Changed code
          /**
            * To create the exponential model, we draw short lines and modify the slope of the lines in each iteration.
            * y = a*math.pow(e,b*x)
            */
          for (increment <- incrementList) {
            val y1 = math.pow(math.E, reg.regression().get._2) * math.pow(math.E, reg.regression().get._1 * increment)
            val y2 = math.pow(math.E, reg.regression().get._2) * math.pow(math.E, reg.regression().get._1 * (increment + expLoopIncrement))
            var listX = Seq((increment.toDouble, increment + expLoopIncrement.toDouble))
            var listY = Seq((y1, y2))
            listX = listX.map {
              _ match {
                case (x1: Double, x2: Double) => ((x1 * width / (bigX - smallX) - width / (bigX - smallX) * smallX + coordinateSystem.getPadding).toDouble,
                  (x2 * width / (bigX - smallX) - width / (bigX - smallX) * smallX + coordinateSystem.getPadding).toDouble)
              }
            }
            listY = listY.map {
              _ match {
                case (x1: Double, x2: Double) => (((-x1 * (height - coordinateSystem.getPadding) / (bigY - smallY) + (height - coordinateSystem.getPadding) / (bigY - smallY) * smallY + height + coordinateSystem.getPadding) - coordinateSystem.getPadding).toDouble,
                  ((-x2 * (height - coordinateSystem.getPadding) / (bigY - smallY) + (height - coordinateSystem.getPadding) / (bigY - smallY) * smallY + height + coordinateSystem.getPadding) - coordinateSystem.getPadding).toDouble)
              }
            }
            if (((y1 >= smallY && y1 <= bigY) && (y2 >= smallY && y2<= bigY)) && ((increment >= smallX && increment <= bigX) &&
              (increment + expLoopIncrement >= smallX && increment +expLoopIncrement <= bigX)) && ((pointWithinLegendConstraints((listX(0)._1, listY(0)._1)) &&
              pointWithinLegendConstraints((listX(0)._2,listY(0)._2))) || !legend)) { //Line is inside the constraints.
              list = list :+ new scalafx.scene.shape.Line() {

                startX = listX(0)._1
                endX = listX(0)._2
                startY = listY(0)._1
                endY = listY(0)._2
                strokeWidth = lineWidth
                stroke = colorPalet(counter)._1
              }
            }
          }
          if (lineWidth > 0) {
            legendData = legendData :+ (colorPalet(counter)._1, "line") //Legend line symbol
          }
        }
        /**
          * Logarithmic model
          */
        case Logarithmic(_, _) => {
          for (point <- mappedValues) {
              if (point != (-1.0, -1.0) && (pointWithinLegendConstraints((point._1, point._2-coordinateSystem.getPadding)) || !legend)) {
                list = list :+ new Rectangle() {
                  x = point._1 - dataSize / 2.0
                  y = point._2 - dataSize / 2.0 - coordinateSystem.getPadding
                  width = dataSize
                  height = dataSize
                  fill = colorPalet(counter)._2
                }
              }
          }
          if (dataSize > 0) {
            legendData = legendData :+ (colorPalet(counter)._2, "square")
          }

          /**
            * y = a*ln(x)+b
            */
          /**We plot 100 lines between the constraints*/
          val logLoopIncrement = (bigX-smallX)/mesh
          //val incrementList = (smallX to bigX by logLoopIncrement)
          //Changed code
          var incrementList = Seq[Double]()
          for (i <- 0 to 100) {
              incrementList = incrementList :+ (smallX + logLoopIncrement*i)
          }
          
          //Changed code

          for (increment <- incrementList) {
            val y1 = reg.regression().get._2 + reg.regression().get._1 * math.log(increment)
            val y2 = reg.regression().get._2 + reg.regression().get._1 * math.log(increment + logLoopIncrement)
            var listX = Seq((increment.toDouble, increment + logLoopIncrement.toDouble))
            var listY = Seq((y1, y2))
            listX = listX.map {
              _ match {
                case (x1: Double, x2: Double) => ((x1 * width / (bigX - smallX) - width / (bigX - smallX) * smallX + coordinateSystem.getPadding).toDouble,
                  (x2 * width / (bigX - smallX) - width / (bigX - smallX) * smallX + coordinateSystem.getPadding).toDouble)
              }
            }
            listY = listY.map {
              _ match {
                case (x1: Double, x2: Double) => (((-x1 * (height - coordinateSystem.getPadding) / (bigY - smallY) + (height - coordinateSystem.getPadding) / (bigY - smallY) * smallY + height + coordinateSystem.getPadding) - coordinateSystem.getPadding).toDouble,
                  ((-x2 * (height - coordinateSystem.getPadding) / (bigY - smallY) + (height - coordinateSystem.getPadding) / (bigY - smallY) * smallY + height + coordinateSystem.getPadding) - coordinateSystem.getPadding).toDouble)
              }
            }
            if (((y1 >= smallY && y1 <= bigY) && (y2 >= smallY && y2<= bigY)) && ((increment >= smallX && increment <= bigX) &&
              (increment + logLoopIncrement >= smallX && increment + logLoopIncrement <= bigX)) && ((pointWithinLegendConstraints((listX(0)._1, listY(0)._1)) &&
              pointWithinLegendConstraints((listX(0)._2,listY(0)._2)))|| !legend)) {
              list = list :+ new scalafx.scene.shape.Line() {

                startX = listX(0)._1
                endX = listX(0)._2
                startY = listY(0)._1
                endY = listY(0)._2
                strokeWidth = lineWidth
                stroke = colorPalet(counter)._1
              }
            }
          }
          if (lineWidth > 0) {
            legendData = legendData :+ (colorPalet(counter)._1, "line") //Legend symbol
          }
        }
      }
      counter += 1 //Color palet
    }
    var newLegend = new Legend(coordinateSystem, legendPos, legendData, legendtext)
    val legendDataPlot = newLegend.plotCurve
    val legendLines = newLegend.plotLegend
    if (legend) { //If user wants legend to be shown.
    new Scene(width, height) {
      content = new Pane() {
        children = Seq(list, visuals._1, visuals._2, labels, legendCurve, legendDataPlot._1, legendDataPlot._2, legendLines, buttonSeq).flatten
      }
    }
  } else {
      new Scene(width, height) {
        content = new Pane() {
          children = Seq(list, visuals._1, visuals._2, labels, buttonSeq).flatten
        }
      }
    }
  }
}
