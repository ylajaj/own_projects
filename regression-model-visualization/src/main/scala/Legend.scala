import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.Black
import scalafx.scene.shape.Shape
import scalafx.scene.text.Text

/**
  * This class creates the legend for the plot.
  */
class Legend(coordinateSystem: CoordinateSystem, legendPos: String, dataset: Seq[(Color, String)], texts: Seq[String]) {

  /**
    * The biggest string length influences on the width of the the legend.
    */
  def biggest: Int = {
    if (texts.size  != 0) {
        var biggest = texts(0).length
        for (text <- texts) {
          if (text.length() > biggest) {
            biggest = text.length
          }
        }
      biggest
    } else
      0
  }
  //Constraints
  def getConstraints: Seq[(Int, Int)] = {
    Seq((coordinateSystem.getPadding +40,coordinateSystem.getPadding +85 + biggest*8),
      (coordinateSystem.getPadding,coordinateSystem.getPadding + (dataset.size - 1) * 20 + 20))
  }

  /**
    * Creating the legend which consists of the black constraint lines, symbols and legend texts.
    */
  //TODO: Support for other places (maybe)
  def plotLegend: (Seq[Shape]) = {
    var legendCurve = Seq[Shape]()
    if (dataset.size != 0) {
      if (legendPos == "nw") {
        legendCurve = legendCurve :+ new scalafx.scene.shape.Line() {
          startX = coordinateSystem.getPadding + 40
          endX = coordinateSystem.getPadding + 85 + biggest * 8
          startY = coordinateSystem.getPadding
          endY = coordinateSystem.getPadding
          strokeWidth = 2
          stroke = Black
        }
        legendCurve = legendCurve :+ new scalafx.scene.shape.Line() {
          startX = coordinateSystem.getPadding + 40
          endX = coordinateSystem.getPadding + 85 + biggest * 8
          startY = coordinateSystem.getPadding + (dataset.size - 1) * 20 + 20
          endY = coordinateSystem.getPadding + (dataset.size - 1) * 20 + 20
          strokeWidth = 2
          stroke = Black
        }
        legendCurve = legendCurve :+ new scalafx.scene.shape.Line() {
          startX = coordinateSystem.getPadding + 40
          endX = coordinateSystem.getPadding + 40
          startY = coordinateSystem.getPadding
          endY = coordinateSystem.getPadding + (dataset.size - 1) * 20 + 20
          strokeWidth = 2
          stroke = Black
        }
        legendCurve = legendCurve :+ new scalafx.scene.shape.Line() {
          startX = coordinateSystem.getPadding + 85 + biggest * 8
          endX = coordinateSystem.getPadding + 85 + biggest * 8
          startY = coordinateSystem.getPadding
          endY = coordinateSystem.getPadding + (dataset.size - 1) * 20 + 20
          strokeWidth = 2
          stroke = Black
        }
      }
      if (legendPos == "nw") {
        legendCurve = legendCurve :+ new scalafx.scene.shape.Line() {
          startX = coordinateSystem.getPadding + 40
          endX = coordinateSystem.getPadding + 85 + biggest * 8
          startY = coordinateSystem.getPadding
          endY = coordinateSystem.getPadding
          strokeWidth = 2
          stroke = Black
        }
        legendCurve = legendCurve :+ new scalafx.scene.shape.Line() {
          startX = coordinateSystem.getPadding + 40
          endX = coordinateSystem.getPadding + 85 + biggest * 6
          startY = coordinateSystem.getPadding + (dataset.size - 1) * 20 + 20
          endY = coordinateSystem.getPadding + (dataset.size - 1) * 20 + 20
          strokeWidth = 2
          stroke = Black
        }
        legendCurve = legendCurve :+ new scalafx.scene.shape.Line() {
          startX = coordinateSystem.getPadding + 40
          endX = coordinateSystem.getPadding + 40
          startY = coordinateSystem.getPadding
          endY = coordinateSystem.getPadding + (dataset.size - 1) * 20 + 20
          strokeWidth = 2
          stroke = Black
        }
        legendCurve = legendCurve :+ new scalafx.scene.shape.Line() {
          startX = coordinateSystem.getPadding + 85 + biggest * 8
          endX = coordinateSystem.getPadding + 85 + biggest * 8
          startY = coordinateSystem.getPadding
          endY = coordinateSystem.getPadding + (dataset.size - 1) * 20 + 20
          strokeWidth = 2
          stroke = Black
        }
      }
    }
    legendCurve
  }

  def plotCurve: (Seq[Shape], Seq[Text]) = {
    var plotData = Seq[Shape]()
    var textList = Seq[Text]()
    if (legendPos == "nw") {
      for (i <- 0 until dataset.size) {
        if (dataset(i)._2 == "line") {
          plotData = plotData :+ new scalafx.scene.shape.Line() {
            startX = coordinateSystem.getPadding + 50
            endX = coordinateSystem.getPadding + 65
            startY = coordinateSystem.getPadding + 10 + i * 20
            endY = coordinateSystem.getPadding + 10 + i * 20
            strokeWidth = 2
            stroke = dataset(i)._1
          }
        }
        if (dataset(i)._2 == "square") {
          plotData = plotData :+ new scalafx.scene.shape.Rectangle() {
            x = coordinateSystem.getPadding + 50
            y = coordinateSystem.getPadding + 10 + i * 20 -3
            width = 6
            height = 6
            fill = dataset(i)._1
          }
        }
        if (i < texts.size) {
          textList = textList :+ new Text(texts(i)) {
            x = coordinateSystem.getPadding + 75
            y = coordinateSystem.getPadding + 10 + i * 20+4
          }
        }
      }
    }

    (plotData, textList)
  }
}
