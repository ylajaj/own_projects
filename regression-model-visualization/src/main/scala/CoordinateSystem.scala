import scalafx.scene.paint.Color.{Black, Grey}
import scalafx.scene.shape.Shape
import scalafx.scene.text.Text

/**
  * This class creates the coordinate system for the plot.
  */
class CoordinateSystem(xConstant: Int, yConstant: Int, gridOn: Boolean, constraintX: (Int, Int), constraintY: (Int, Int),
                       multiplier: (Double, Double), legendWidth: Double, legendSize: Int, legend: Boolean) {

  private val padding = 80 //Space outside of the coordinate system.

  //We do not want to draw grid lines inside the legend block.
  val legendConstraints: Seq[(Double,Double)] = Seq((this.getPadding +40,this.getPadding),
    (this.getPadding +85 + legendWidth*8,this.getPadding + ((legendSize)-1) * 20 + 20))


  def getPadding: Int = padding // Getter for padding.


  var numberLines = Seq[Shape]() //All the lines: axes and grid
  var textList = Seq[Text]() //Numbers outside the coordinate system

  /**
    * Drawing the axes and the grid.
    */
  var xAxis: Shape = new scalafx.scene.shape.Line() {
    startX = padding
    endX= xConstant + padding
    startY = yConstant
    endY = yConstant
    strokeWidth = 2
    stroke = Black
  }
  var yAxis: Shape = new scalafx.scene.shape.Line() {
    startX = padding
    endX= padding
    startY = yConstant
    endY = padding
    strokeWidth = 2
    stroke = Black
  }
  var a1xAxis: Shape = new scalafx.scene.shape.Line() {
    startX = xConstant + padding
    endX= xConstant + padding - 5
    startY = yConstant
    endY = yConstant-5
    strokeWidth = 2
    stroke = Black
  }
  var a2xAxis: Shape = new scalafx.scene.shape.Line() {
    startX = padding
    endX= 5 + padding
    startY = padding
    endY = 5 + padding
    strokeWidth = 2
    stroke = Black
  }
  var a1yAxis: Shape = new scalafx.scene.shape.Line() {
    startX = padding
    endX= -5 + padding
    startY = padding
    endY = 5 + padding
    strokeWidth = 2
    stroke = Black
  }
  var a2yAxis: Shape = new scalafx.scene.shape.Line() {
    startX = xConstant + padding
    endX= xConstant + padding - 5
    startY = yConstant
    endY = yConstant + 5
    strokeWidth = 2
    stroke = Black
  }

  for (i <- 0 until 6) { //Little lines above the numbers that show the exact position of the value.
    if (i != 0) {
      if(i != 5) {
        val line: Shape = new scalafx.scene.shape.Line() {
          startX = padding + i * (xConstant / 5.0).toInt
          endX = padding + i * (xConstant / 5.0).toInt
          startY = yConstant
          endY = yConstant - 5
          strokeWidth = 2
          stroke = Black
        }
        numberLines = numberLines :+ line
      }
      if (gridOn) { //If user wants the grid.
        for (yvalue <- padding.toInt to (yConstant-6).toInt) {
          if (!(((padding + i * (xConstant / 5.0).toInt).toDouble > legendConstraints(0)._1.toDouble) &&
            ((padding + i * (xConstant / 5.0).toInt).toDouble < legendConstraints(1)._1.toDouble) &&
            ((yvalue) > legendConstraints(0)._2.toDouble) && ((yvalue) < legendConstraints(1)._2.toDouble)) || !legend) {

            numberLines = numberLines :+ new scalafx.scene.shape.Line() {
              startX = padding + i * (xConstant / 5.0).toInt
              endX = padding + i * (xConstant / 5.0).toInt
              startY = yvalue
              endY = yvalue + 1
              strokeWidth = 0.5
              stroke = Grey
            }
          }
        }
      }
    }
    val adderX = ((constraintX._2-constraintX._1))/5
    val text = new Text((constraintX._1+i*adderX).toInt.toString) {
      x = padding + i * (xConstant / 5.0).toInt-text.value.length*4
      y = yConstant + 20
    }
    textList = textList :+ text
  }
  for (i <- 0 until 6) { //Little lines next to the number that show the exact position of the value.
    if (i != 0) {
      if (i != 5) {
        var line: Shape = new scalafx.scene.shape.Line() {
          startX = padding
          endX = padding + 5
          startY = yConstant - i * ((yConstant - padding) / 5.0).toInt-1
          endY = yConstant - i * ((yConstant - padding) / 5.0).toInt-1
          strokeWidth = 2
          stroke = Black
        }
        numberLines = numberLines :+ line
      }
      if (gridOn) { //If user wants the grid
        for (xvalue <- padding to (xConstant + padding -1).toInt) {
          if (!(((xvalue) > legendConstraints(0)._1.toDouble) && ((xvalue) < legendConstraints(1)._1.toDouble) &&
            ((yConstant - i * ((yConstant - padding) / 5.0).toInt-1) > legendConstraints(0)._2) &&
            ((yConstant - i * ((yConstant - padding) / 5.0).toInt-1) < legendConstraints(1)._2)) || !legend) {

            numberLines = numberLines :+ new scalafx.scene.shape.Line() {
              startX = xvalue
              endX = xvalue +1
              startY = yConstant - i * ((yConstant - padding) / 5.0).toInt-1
              endY = yConstant - i * ((yConstant - padding) / 5.0).toInt-1
              strokeWidth = 0.5
              stroke = Grey
            }
          }
        }
      }
    }
    val adderY = ((constraintY._2-constraintY._1))/5
    val text = new Text((constraintY._1+i*adderY).toInt.toString) {
      x = padding - 20-text.value.length*4
      y = yConstant - i * ((yConstant - padding) / 5.0).toInt+4
    }
    textList = textList :+ text

  }
  /**
    * If the mapping of the x values of the data set was changed.
    */
  if (multiplier._1 != 1.0) {
    if (multiplier._1 < 1.0) {
      var zeros = multiplier._1.toString.filterNot(_ == '.').takeWhile(_ == '0').length
      if (multiplier._1.toString.contains('E')) {
        zeros = multiplier._1.toString().split('-')(1).toInt
      }
      val text = new Text("x10" + "^" + zeros) {
        x = padding + 5 * (xConstant / 5.0).toInt-text.value.length*4
        y = yConstant + 45
      }
      textList = textList :+ text
    } else {
      var zeros = multiplier._1.toInt.toString().count(_ == '0')
      if (multiplier._1.toString.contains('E')) {
        zeros = multiplier._1.toString().split('E')(1).toInt
      }
      val text = new Text("x10" + "^"+"(-" + zeros+")") {
        x = padding + 5* (xConstant / 5.0).toInt-text.value.length*4
        y = yConstant + 45
      }
      textList = textList :+ text
    }
  }

  /**
    * If the mapping of the y values of the data set was changed.
    */
  if (multiplier._2 != 1.0) {
    if (multiplier._2 < 1.0) {
      var zeros = multiplier._2.toString.filterNot(_ == '.').takeWhile(_ == '0').length
      if (multiplier._2.toString.contains('E')) {
        zeros = multiplier._2.toString().split('-')(1).toInt
      }
      val text = new Text("x10" + "^" + zeros) {
        x = padding
        y = padding - 15
      }
      textList = textList :+ text
    } else {
      var zeros = multiplier._2.toInt.toString().count(_ == '0')
      if (multiplier._2.toString.contains('E')) {
        zeros = multiplier._2.toString().split('E')(1).toInt
      }
      val text = new Text("x10" + "^"+"(-" + zeros+")") {
        x = padding
        y = padding - 15
      }
      textList = textList :+ text
    }
  }

  numberLines = numberLines :+ xAxis //x axis
  numberLines = numberLines :+ yAxis //y axis
  numberLines = numberLines :+ a1xAxis //arrow
  numberLines = numberLines :+ a1yAxis //arrow
  numberLines = numberLines :+ a2xAxis //arrow
  numberLines = numberLines :+ a2yAxis //arrow

  /**
    * Returns everything that is going to be drawn.
    */
  def returnVisuals: (Seq[Shape], Seq[Text]) = (numberLines,textList)

}

