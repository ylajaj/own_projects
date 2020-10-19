import java.io._

import UI.stage
import org.apache.poi.ss.usermodel.{DataFormatter, WorkbookFactory}
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType

/**
  * Class Data provides the data reading methods and some other functionalities, such as the
  * biggest and smallest values of the data set.
  */
class Data {

  /** The place for the data set. */
  private var data: Option[Seq[(Double, Double)]] = None

 /**If the range of the data set is too small or too big, it is mapped to a new range with these multipliers*/
  private var multiplierX = 1.0
  private var multiplierY = 1.0

  /**
    * When we change the data, we have to reset the multipliers.
    */
  def changeData(seq: Seq[(Double, Double)]): Unit = {
    multiplierX = 1.0
    multiplierY = 1.0
    this.data = Some(seq)
  }

//TODO: Is the method removeData useful? Does it work?
  /**
    * Removes measurement errors from the data set.
    */
  def removeData(seq: Seq[Int]): Unit = {

    if (data != None) {
      val removedOption = data.get
      val correctedData = {
        for(value <- seq; i <- 0 until removedOption.length; if (value != i))
          yield removedOption(i)
      }
      data = Some(correctedData)
    }

  }

  /**
    * Reading txt files into a sequence of doubles. The txt file must be in the correct form where there are two values
    * in each of the lines and the values are separated by whitespaces.
    */
  def readTxt(sourceFile: File): Option[Seq[(Double, Double)]] = {

    try { //Trying to find the file
      val fileIn = new FileReader(sourceFile)
      val linesIn = new BufferedReader(fileIn)
      try {
        var resList = Seq[String]()
        var oneLine: String = null
        while ({oneLine = linesIn.readLine(); oneLine != null}) {
          if (!oneLine.trim().startsWith("#") && !oneLine.trim().isEmpty) //The lines may contain some jibberish.
          resList = resList :+ oneLine.trim.replaceAll("\\s+", " ")

        }
        //We create a tuple sequence for the values.
        var wrongInput = false
        val finalList = resList.map {
          case x: String if (x.trim.split(" ").size != 2) => {
            wrongInput = true
            (0.00000,0.00000)
          }
          case x: String if (!isDouble(x.split(" ")(0).trim) || !isDouble(x.split(" ")(1).trim)) => {
            wrongInput = true
            (0.00000,0.00000)
          }
          case x: String => (x.trim().split(" ")(0).toDouble, x.trim.split(" ")(1).toDouble)

          }
        val newList  = finalList//.filter(_!=(0.00000,0.00000))
        if (newList.contains((0.00000,0.00000))) {
          val alert = new Alert(AlertType.Warning) {
            initOwner(stage)
            title = "Wrong type of data"
            headerText = "Your text file contains wrong type of data!"
            contentText = "Every line of your text file must be in form: Double Double. Lines like, 0.01, 0.02, are wrong. Wrong type of lines were changed to: 0.00000 0.00000."
          }.showAndWait()
        }
        changeData(newList)
        Some(finalList)
      } finally {
        fileIn.close()
        linesIn.close()
      }
    } catch {
      case e: FileNotFoundException => println("File not found"); None
    }
  }
  //TODO: readTxtArea is removable
  def readTxtArea(source: String): Option[Seq[(Double, Double)]] = {
        var resList = source.split("\n")
        val finalList = resList.map {case x: String if (!x.trim.isEmpty) => (x.trim.split(" ")(0).toDouble, x.trim.split(" ")(1).toDouble)}
        changeData(finalList)
        Some(finalList)
  }

  /**
    * Reading xlsx files using FileInputStream and WorkbookFactory.
    */
  //TODO: Error for an empty file
  def readExcelFile(source: File, cols: (Int, Int)): Unit = {
    var seqX = Seq[Double]() //List for the x values
    var seqY = Seq[Double]() //List for the y values

    val inputStream = new FileInputStream(source)

    val workbook = WorkbookFactory.create(inputStream)
    val sheet = workbook.getSheetAt(0)
    val dataFormatter = new DataFormatter()

    /**
      * Reading the numeric values of the file
      */
    val rowsCount = sheet.getLastRowNum
    for (i <- 0 until rowsCount) {
      val row = sheet.getRow(i)
      val colCounts = row.getLastCellNum
      for (j <- 0 to colCounts) {
        val cell = row.getCell(j)
        val data = dataFormatter.formatCellValue(cell).replace(',', '.').trim // Replacing all the commas with dots.
        if (isDouble(data)) {
          if (j == cols._1) {
            seqX = seqX :+ cell.getNumericCellValue.toDouble
          }
          if (j == cols._2) {
              seqY = seqY :+ cell.getNumericCellValue.toDouble
            }
        }
      }
    }
      val dataPoints = seqX.zip(seqY) //Combining the x and y values.
      changeData(dataPoints)
      data = Some(dataPoints)
  }

  /**Reading csv files. These files are just text fields with commas separating the columns. The user selects the columns
    * to be imported.*/
  def readCSV(source: File, colNum: (Int, Int)): Unit = {
    val bufferedSource = io.Source.fromFile(source)
    var columns = Seq[(Double, Double)]()
    for (line <- bufferedSource.getLines()) {
      val cols = line.split(",").map(_.trim) //Separating the columns from one line
      if (isDouble(cols(colNum._1)) && isDouble(cols(colNum._2))) { //Choosing the columns that the user wants.

        columns = columns :+ (cols(colNum._1).toDouble,cols(colNum._2).toDouble) //Adding the data pair into the sequence.
      }
    }
    bufferedSource.close()
    data = Some(columns)
  }

  /**This function counts the number of columns that the user can select from. A -1 is returned if the file does not contain
    * any columns.
    */
  def csvCols(source: File): Int = {
    val bufferedSource = io.Source.fromFile(source)
    if (bufferedSource.getLines().nonEmpty) {
      bufferedSource.getLines().next().split(",").map(_.trim).length
    } else
      -1
  }
//TODO: This function should check the number of columns in the file and return -1 if no columns were detected.
  def excelCols(source: File): Int = {
    val inputStream = new FileInputStream(source)
    if (inputStream != null) { //TODO: Empty excel file
      val workbook = WorkbookFactory.create(inputStream)
      var sheet = workbook.getSheetAt(0)
      sheet.getRow(0).getLastCellNum
    } else
      -1
  }

  def getData : Option[Seq[(Double, Double)]] = this.data //Returns the data set

  /**
    * Returns the biggest x value and y value.
    */
  def biggest: Option[(Double, Double)] = {
    var biggestX = data.get(0)._1
    var biggestY = data.get(0)._2

    for (point <- data.get) {
      if (biggestX < point._1) {
        biggestX = point._1
      }
      if (biggestY < point._2) {
        biggestY = point._2
      }
    }
    Some(biggestX,biggestY)
  }

  /**
    * Returns the smallest x value and y value.
    */
  def smallest: Option[(Double, Double)] = {
    var smallestX = data.get(0)._1
    var smallestY = data.get(0)._2

    for (point <- data.get) {
      if (smallestX > point._1) {
        smallestX = point._1
      }
      if (smallestY > point._2) {
        smallestY = point._2
      }
    }
    Some(smallestX,smallestY)
  }

  /**
    * Setting the appropriate range for the data set. If the range is too small, we increase it. If the range is
    * too big, we decrease it. The order of the data set has to be between 10 and 1000.
    */
  def changeMultiplierX(multiplier: Double): Unit = {
    if (multiplier*(biggest.get._1-smallest.get._1) < 10) {
      multiplierX = multiplier*10
      changeMultiplierX(multiplierX)
    }
    if (multiplier*(biggest.get._1-smallest.get._1) > 1000) {
      multiplierX = multiplier*0.1
      changeMultiplierX(multiplierX)
    }
  }
  def changeMultiplierY(multiplier: Double): Unit = {
    if (multiplier*(biggest.get._2-smallest.get._2) < 10) {
      multiplierY = multiplier*10
      changeMultiplierY(multiplierY)
    }
    if (multiplier*(biggest.get._2-smallest.get._2) > 1000) {
      multiplierY = multiplier*0.1
      changeMultiplierY(multiplierY)
    }
  }

  def getMultipliers: (Double, Double) = (multiplierX, multiplierY) //Returns the two multipliers.

  /**
    * This function makes the range of the data bigger or smaller.
    */
  def multiplyData(x: Double, y: Double): Unit =  {
    val seq = data.get.map{ case (x1: Double, x2: Double) => (x1*x, x2*y)}
    this.data = Some(seq)
  }

  /**Determines wheter the string is an integer or not*/
  def isInteger(str: String): Boolean = {
    val positiveInt = str.forall(_.isDigit)
    if (str.length > 1) {
      positiveInt || (str(0) == '-' && str.substring(1).forall(_.isDigit))
    }
    else if (str.length == 1) {
      str(0).isDigit
    }
    else false
  }

  /**Determines wheter the string is a double or not*/
  def isDouble (str: String): Boolean = {
    var splittedString = str.split('.')

    if (splittedString.length == 1) {
      isInteger(splittedString(0))
    }
    else if (splittedString.length == 2) {
      if (splittedString(0)(0) == '-') {
        splittedString = str.filterNot(_ == '-').split('.')
      }
      (splittedString(0)(0) == '0' && isInteger(splittedString(1))) || (isInteger(splittedString(0))
        && isInteger(splittedString(1)) && splittedString(0)(0) != '0')
    }
    else false
  }
}
