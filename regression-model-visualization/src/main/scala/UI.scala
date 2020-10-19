import scalafx.application.JFXApp
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.layout.Pane
import scalafx.scene.control._
import scalafx.scene.text.{Font, Text}
import scalafx.Includes._
import scalafx.stage.FileChooser
import javafx.scene.{control => jfxsc}
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType


/**
  * This is the user interface.
  */
object UI extends JFXApp {

 /**Class for the table view in the info window*/
  case class Info(title: String, regression: String, constantB: Double, constantK: Double, minX: Double, maxX: Double, minY: Double, maxY: Double)

  var buttonClicked = false
  var legend = false
  var lineWidth = 1 //Default line width for the plot
  var dataSize = 4 //Default data point size for the plot
  var plotSizeValue = "Small" //Default plot window size
  val font = new Font("System Regular", 15.0) //Font for the titles
  var infoTexts = Seq[Text]() //Texts for the info window


 /**Determines if the string is an integer*/
  def isInteger(str: String): Boolean = {
    if (str.nonEmpty) {
      val positiveInt = str.forall(_.isDigit)
      if (str.length > 1) {
        positiveInt || (str(0) == '-' && str.substring(1).forall(_.isDigit))
      }
      else if (str.length == 1) {
        str(0).isDigit
      }
      else false
    } else false
  }

 /**Determines if the string is a double*/
  def isDouble (str: String): Boolean = {
    if (str.nonEmpty) {
      var splittedString = str.split('.')

      if (splittedString.length == 1) {
        isInteger(splittedString(0))
      }
      else if (splittedString.length == 2) {
        if (splittedString(0)(0) == '-') {
          splittedString = str.substring(1).split('.')
        }
        (splittedString(0)(0) == '0' && splittedString(0).length == 1 && isInteger(splittedString(1))) || (isInteger(splittedString(0)) && isInteger(splittedString(1)) && splittedString(0)(0) != '0')
      }
      else false
    } else false
  }

  /**Determines if the file is already imported to the program*/
  def fileNotIncluded(str: String): Boolean = regressionModels.forall(_.getTitle != str)

  var regressionModels = Seq[Regression]() //List of the imported regression models

  var stageWidth = 700
  var stageHeight = 600
  var gridOn = false //Default setting for gridOn.
  var legendBoolean = false //Default setting for legend.


  var buttonStart = new scalafx.scene.control.Button("Plot all files") { //Plot all files button.
    layoutX = 520
    layoutY = 235
  }
  buttonStart.setPrefHeight(30)
  buttonStart.setPrefWidth(180)
  var chosenStart = new scalafx.scene.control.Button("Plot chosen files") { //Plot chosen files button
    layoutX = 520
    layoutY = 275
  }
  chosenStart.setPrefHeight(30)
  chosenStart.setPrefWidth(180)
  var checkBoxLegend = new scalafx.scene.control.CheckBox("Legend on") { //Checkbox for legend
    layoutX = 370
    layoutY = 110
  }
  var checkBoxGridOn = new scalafx.scene.control.CheckBox("Grid on") { //Checkbox for grid on
    layoutX = 370
    layoutY = 80
  }
  var parametersText = new Text("Parameters:") {
    layoutX = 30
    layoutY = 60
  }
  parametersText.setFont(font)

  var adjustments = new Text("Adjustments:") {
    layoutX = 370
    layoutY = 60
  }
  adjustments.setFont(font)

  var plotSize = new ComboBox[String](Seq("Small", "Medium", "Large")) {
    layoutX = 370
    layoutY = 160
  }
  var plotSizeText = new Text("Plot size") {
    layoutX = 370
    layoutY = 150
  }

  var text = new Text("Textfields:") {
    layoutX = 200
    layoutY = 60
  }
  text.setFont(font)

  var textXs = new scalafx.scene.control.TextField() {
    layoutX = 30
    layoutY = 100

  }
  var textXb = new scalafx.scene.control.TextField() {
    layoutX =30
    layoutY = 160
  }
  var textYs = new scalafx.scene.control.TextField() {
    layoutX = 30
    layoutY = 220
  }
  var textYb = new scalafx.scene.control.TextField() {
    layoutX =30
    layoutY = 280
  }
  var textsx = new Text("x-axis start*") {
    layoutX = 30
    layoutY = 90
  }
  var textbx = new Text("x-axis end*") {
    layoutX = 30
    layoutY = 150
  }
  var textsy = new Text("y-axis start*") {
    layoutX = 30
    layoutY = 210
  }
  var textby = new Text("y-axis end*") {
    layoutX = 30
    layoutY = 270
  }
  textXs.setPrefWidth(120)
  textXb.setPrefWidth(120)
  textYs.setPrefWidth(120)
  textYb.setPrefWidth(120)

  var xlabelField = new scalafx.scene.control.TextField() {
    layoutX = 200
    layoutY = 100
  }
  var ylabelField = new scalafx.scene.control.TextField() {
    layoutX = 200
    layoutY = 160
  }
  var legendText = new scalafx.scene.control.TextField() {
    layoutX = 200
    layoutY = 220
  }
  var titleText = new scalafx.scene.control.TextField() {
    layoutX =200
    layoutY = 280
  }
  ylabelField.setPrefWidth(120)
  xlabelField.setPrefWidth(120)
  titleText.setPrefWidth(120)
  legendText.setPrefWidth(120)

  var txtTitle = new Text("Title") {
    layoutX = 200
    layoutY = 270
  }
  var legendtxt = new Text("Legend text") {
    layoutX = 200
    layoutY = 210
  }
  var xlabelText= new Text("x label") {
    layoutX = 200
    layoutY = 90
  }
  var ylabelText = new Text("y label") {
    layoutX = 200
    layoutY = 150
  }

  var comboBox = new scalafx.scene.control.ComboBox(Seq("0","1", "2","3", "4", "6","8", "10")) { //Combobox for data point size
      layoutX = 370
      layoutY = 220
  }
  var comboBoxLine = new scalafx.scene.control.ComboBox(Seq("0", "1")) { //Combobox for linewidth
    layoutX = 370
    layoutY = 280
  }
  var lineText = new Text("Line width") {
    layoutX = 370
    layoutY = 270
  }

  var datapointText = new Text("Datapoint size") {
    layoutX = 370
    layoutY = 210
  }
  var dataFiles = new Text("Data files:") {
    layoutX = 520
    layoutY = 60
  }
  dataFiles.setFont(font)




  val list = new scalafx.scene.control.ListView[String]() {
    layoutX = 520
    layoutY = 90
  }
  list.setPrefWidth(180)
  list.setEditable(true)
  list.setPrefHeight(130)
  list.getSelectionModel.setSelectionMode(jfxsc.SelectionMode.MULTIPLE)


  var plotWidthText = new Text("Plot width") {
    layoutX = 370
    layoutY = 170
  }

  var plotHeighText = new Text("Plot height") {
    layoutX = 370
    layoutY = 230
  }
  var plotWidth = new TextField() {
    layoutX = 370
    layoutY = 180
  }
  var plotHeight = new TextField() {
    layoutX = 370
    layoutY = 240
  }
  plotWidth.setPrefWidth(120)
  plotHeight.setPrefWidth(120)

  val menuBar = new MenuBar
  menuBar.setPrefWidth(stageWidth*1.0+30)

  val fileMenu = new Menu("Options")
  val help = new MenuItem("Help")
  val importItem = new Menu("Import data")
  val linearImport = new MenuItem("Linear")
  val expImport = new MenuItem("Exponential")
  val logImport = new MenuItem("Logarithmic")
  val remove = new MenuItem("Remove all files")
  val regressionInfo = new MenuItem("Info about the files")
  importItem.items = Seq(linearImport, expImport, logImport)
  fileMenu.items = Seq(importItem,regressionInfo,help,remove)
  menuBar.menus = Seq(fileMenu)

  regressionInfo.onAction = (e:ActionEvent) => { //Goes to the info page
      var data = ObservableBuffer[Info]()
      for(reg <- regressionModels) {
        val newData = new Data
        newData.changeData(reg.seq)
        reg match {
          case Linear(_,_) => data = data :+ Info(reg.getTitle,"Linear", reg.regression().get._2, reg.regression().get._1, (newData.smallest.get._1*1000).round/1000.0,(newData.biggest.get._1*1000).round/1000.0,(newData.smallest.get._2*1000).round/1000.0,(newData.biggest.get._2*1000).round/1000.0)
          case Exponential(_,_) => data = data :+ Info(reg.getTitle,"Exponential", math.pow(math.E,reg.regression().get._2), reg.regression().get._1,(newData.smallest.get._1*1000).round/1000.0,(newData.biggest.get._1*1000).round/1000.0,(newData.smallest.get._2*1000).round/1000.0,(newData.biggest.get._2*1000).round/1000.0)
          case Logarithmic(_,_) => data = data :+ Info(reg.getTitle,"Logarithmic", reg.regression().get._2, reg.regression().get._1,(newData.smallest.get._1*1000).round/1000.0,(newData.biggest.get._1*1000).round/1000.0,(newData.smallest.get._2*1000).round/1000.0,(newData.biggest.get._2*1000).round/1000.0)
        }

      }
    val table = new TableView(data) {
      layoutY = 30
    }
    table.setStyle("-fx-focus-color: transparent;")
    table.setPrefWidth(900)
    table.setPrefHeight(400)
    val col1 = new TableColumn[Info, String]("Name")
    val col2 = new TableColumn[Info, String]("Regression")
    col1.cellValueFactory = x => StringProperty(x.value.title)
    col2.cellValueFactory= x => StringProperty(x.value.regression)
    val col3 = new TableColumn[Info, String]("Constant (b)")
    col3.cellValueFactory = x => StringProperty(x.value.constantB.toString)
    val col4 = new TableColumn[Info, String]("Constant (k)")
    col4.cellValueFactory = x => StringProperty(x.value.constantK.toString)
    val col5 = new TableColumn[Info, String]("min x")
    col5.cellValueFactory = x => StringProperty(x.value.minX.toString)
    val col6 = new TableColumn[Info, String]("max x")
    col6.cellValueFactory = x => StringProperty(x.value.maxX.toString)
    val col7 = new TableColumn[Info, String]("min y")
    col7.cellValueFactory = x => StringProperty(x.value.minY.toString)
    val col8 = new TableColumn[Info, String]("max y")
    col8.cellValueFactory = x => StringProperty(x.value.maxY.toString)
    table.columns ++= List(col1, col2, col3, col4,col5, col6, col7,col8)
    var infoScene = new Scene() {
      content = new Pane() {
        var infoBackMenuBar = new MenuBar
        infoBackMenuBar.setPrefWidth(900)
        var menu = new Menu("Options")
        infoBackMenuBar.menus = Seq(menu)
        var item = new MenuItem("Back")
        menu.items = Seq(item)




        item.onAction = (e:ActionEvent) => { //Return to the starting scene
          infoTexts = Seq[Text]()
          stage.width = 730
          stage.height = 370
          stage.setScene(startScene)
          stage.show()
        }

        children = Seq(Seq(infoBackMenuBar),Seq(table)).flatten
      }
    }
    stage.width = 900
    stage.height = 400
    stage.setScene(infoScene)
    stage.show()
  }

  help.onAction = (e:ActionEvent) => { //Goes to the help window
    var linearText = new Text("Linear regression:") {
      layoutX = 30
      layoutY = 60
    }
    linearText.setFont(font)
    var expText = new Text("Exponential regression:") {
      layoutX = 30
      layoutY = 160
    }
    expText.setFont(font)
    var logText = new Text("Logarithmic regression:") {
      layoutX = 30
      layoutY = 260
    }
    logText.setFont(font)
    val linearExp = new Text("Linear regression produces a curve y = kx + b where k and b are some constants.") {
      layoutX = 30
      layoutY = 100
    }
    val expExp = new Text("Exponential regression produces a curve y = b*e^(kx) where k and b are some constants.") {
      layoutX = 30
      layoutY = 200
    }
    val logExp = new Text("Logarithmic regression produces a curve y = b+k*ln(x) where k and b are some constants.") {
      layoutX = 30
      layoutY = 300
    }

    val menuBar = new MenuBar
    menuBar.setPrefWidth(730)
    val menu = new Menu("Options")
    menuBar.menus = Seq(menu)
    val item = new MenuItem("Back")
    menu.items = Seq(item)

    var helpScene = new Scene() {
      content = new Pane() {
        children = Seq(linearText,expText,logText,menuBar, linearExp,logExp,expExp)
      }
      item.onAction = (e:ActionEvent) => { //Returns to the starting scene
        infoTexts = Seq[Text]()
        stage.width = 730
        stage.height = 370
        stage.setScene(startScene)
        stage.show()
      }
    }
    stage.width = 730
    stage.height = 370
    stage.setScene(helpScene)
    stage.show()
  }


  linearImport.onAction = (e:ActionEvent) => { //Importing linear regression data
    val fileChooser = new FileChooser()
    val selectedFile = fileChooser.showOpenDialog(stage)
    if (selectedFile != null) { //Cancel selected
      if (fileNotIncluded(selectedFile.getName.mkString)) { //File already exists
        val fileType = selectedFile.getName.mkString.split('.').last
        if (fileType == "txt" || fileType == "xlsx" || fileType == "csv") { //Wrong filetype check
          if (fileType == "txt") {
            val data = new Data
            val readData = data.readTxt(selectedFile)
            var regression = Linear(data.getData.get, selectedFile.getName.mkString)
            regressionModels = regressionModels :+ regression
          }
          if (fileType == "xlsx") {
            val data = new Data
            val colCount = data.excelCols(selectedFile) - 1
            if (colCount != -2) { //Empty file
              val dialog = new TextInputDialog(defaultValue = "0, 1") {
                initOwner(stage)
                title = "Confirm columns"
                contentText = "Separate the columns that you want with a comma! Your file contains columns from 0 to " + colCount + "."
                headerText = "Choose the columns of your data file."

              }

              val result: Seq[String] = dialog.showAndWait().get.split(',')
              if (dialog.showAndWait().isDefined) {
              if (result.length == 2) { //Two columns selected
                if (isInteger(result(0).trim) && isInteger(result(1).trim) && result(0).trim.toInt >= 0 && result(0).trim.toInt <= colCount && result(1).trim.toInt >= 0 && result(1).trim.toInt <= colCount) {
                  data.readExcelFile(selectedFile, (result(0).trim.toInt, result(1).trim.toInt))
                  if (!data.getData.get.isEmpty && fileNotIncluded(selectedFile.getName.mkString + result(0).trim + result(1).trim)) { //Reading process failed
                    var regression = Linear(data.getData.get, selectedFile.getName.mkString + result(0).trim + result(1).trim)
                    regressionModels = regressionModels :+ regression
                    list.getItems.add(selectedFile.getName.mkString + result(0).trim + result(1).trim)
                  } else {
                    val alert = new Alert(AlertType.Error) {
                      initOwner(stage)
                      title = "Error"
                      headerText = "The columns you chose did not contain doubles or this file already exists!"
                    }.showAndWait()
                  }
                } else {
                  val alert = new Alert(AlertType.Error) {
                    initOwner(stage)
                    title = "Error"
                    headerText = "You did not choose correct type of columns!"
                  }.showAndWait()
                }
              } else {
                val alert = new Alert(AlertType.Error) {
                  initOwner(stage)
                  title = "Error"
                  headerText = "You did not choose correct type of columns!"
                }.showAndWait()
              }
            }
          } else {
              val alert = new Alert(AlertType.Error) {
                initOwner(stage)
                title = "Error"
                headerText = "Your file was empty!"
              }.showAndWait()
          }

          }
          if (fileType == "csv") {
            val data = new Data
            val colCount = data.csvCols(selectedFile) - 1
            if (colCount != -2) { //Empty file
              val dialog = new TextInputDialog(defaultValue = "0, 1") {
                initOwner(stage)
                title = "Confirm columns"
                contentText = "Separate the columns that you want with a comma! Your file contains columns from 0 to " + colCount + "."
                headerText = "Choose the columns of your data file."

              }
              val result: Seq[String] = dialog.showAndWait().get.split(',')
              if (dialog.showAndWait().isDefined) {
              if (result.length == 2) { //Two columns selected
                if (isInteger(result(0).trim) && isInteger(result(1).trim) && result(0).trim.toInt >= 0 && result(0).trim.toInt <= colCount && result(1).trim.toInt >= 0 && result(1).trim.toInt <= colCount) {
                  data.readCSV(selectedFile, (result(0).trim.toInt, result(1).trim.toInt))
                  if (!data.getData.get.isEmpty && fileNotIncluded(selectedFile.getName.mkString + result(0).trim + result(1).trim)) { //Reading failed
                    var regression = Linear(data.getData.get, selectedFile.getName.mkString + result(0).trim + result(1).trim)
                    regressionModels = regressionModels :+ regression
                    list.getItems.add(selectedFile.getName.mkString + result(0).trim + result(1).trim)
                  } else {
                    val alert = new Alert(AlertType.Error) {
                      initOwner(stage)
                      title = "Error"
                      headerText = "The columns you chose did not contain doubles or this file already exists!"
                    }.showAndWait()
                  }
                } else {
                  val alert = new Alert(AlertType.Error) {
                    initOwner(stage)
                    title = "Error"
                    headerText = "You did not choose correct type of columns!"
                  }.showAndWait()
                }
              } else {
                val alert = new Alert(AlertType.Error) {
                  initOwner(stage)
                  title = "Error"
                  headerText = "You did not choose correct type of columns!"
                }.showAndWait()
              }
            }
          } else {
              val alert = new Alert(AlertType.Error) {
                initOwner(stage)
                title = "Error"
                headerText = "Your file was empty!"
              }.showAndWait()
          }

          }
          if (fileType != "csv" && fileType != "xlsx") { //txt files
            list.getItems.add(selectedFile.getName.mkString)
          }
        } else {
          val alert = new Alert(AlertType.Error) {
            initOwner(stage)
            title = "Error"
            headerText = "This program only supports .txt, .xlsx and .csv files!"
          }.showAndWait()
        }
      } else {
        val alert = new Alert(AlertType.Error) {
          initOwner(stage)
          title = "Error"
          headerText = "This file already exists!"
        }.showAndWait()
      }
    }
  }
  /**
    * The expImport handles the exponential model import. It works exactly the same way as the linear import.
    */
  expImport.onAction = (e:ActionEvent) => {
    val fileChooser = new FileChooser()
    val selectedFile = fileChooser.showOpenDialog(stage)
    if (selectedFile != null) {
      if (fileNotIncluded(selectedFile.getName.mkString)) {
        val fileType = selectedFile.getName.mkString.split('.').last
        if (fileType == "txt" || fileType == "xlsx" || fileType == "csv") {
          if (fileType == "txt") {
            val data = new Data
            val readData = data.readTxt(selectedFile)
            var regression = Exponential(data.getData.get, selectedFile.getName.mkString)
            regressionModels = regressionModels :+ regression
          }
          if (fileType == "xlsx") {
            val data = new Data
            val colCount = data.excelCols(selectedFile) - 1
            if (colCount != -2) {
              val dialog = new TextInputDialog(defaultValue = "0, 1") {
                initOwner(stage)
                title = "Confirm columns"
                contentText = "Separate the columns that you want with a comma! Your file contains columns from 0 to " + colCount + "."
                headerText = "Choose the columns of your data file."

              }
              val result: Seq[String] = dialog.showAndWait().get.split(',')
              if (dialog.showAndWait().isDefined) {
              if (result.length == 2) {
                if (isInteger(result(0).trim) && isInteger(result(1).trim) && result(0).trim.toInt >= 0 && result(0).trim.toInt <= colCount && result(1).trim.toInt >= 0 && result(1).trim.toInt <= colCount) {
                  data.readExcelFile(selectedFile, (result(0).trim.toInt, result(1).trim.toInt))
                  if (!data.getData.get.isEmpty && fileNotIncluded(selectedFile.getName.mkString + result(0).trim + result(1).trim)) {
                    var regression = Exponential(data.getData.get, selectedFile.getName.mkString + result(0).trim + result(1).trim)
                    regressionModels = regressionModels :+ regression
                    list.getItems.add(selectedFile.getName.mkString + result(0).trim + result(1).trim)
                  } else {
                    val alert = new Alert(AlertType.Error) {
                      initOwner(stage)
                      title = "Error"
                      headerText = "The columns you chose did not contain doubles or this file already exists!"
                    }.showAndWait()
                  }
                } else {
                  val alert = new Alert(AlertType.Error) {
                    initOwner(stage)
                    title = "Error"
                    headerText = "You did not choose correct type of columns!"
                  }.showAndWait()
                }
              } else {
                val alert = new Alert(AlertType.Error) {
                  initOwner(stage)
                  title = "Error"
                  headerText = "You did not choose correct type of columns!"
                }.showAndWait()
              }
            }
          } else {
            val alert = new Alert(AlertType.Error) {
              initOwner(stage)
              title = "Error"
              headerText = "Your file was empty!"
            }.showAndWait()
          }
          }
          if (fileType == "csv") {
            val data = new Data
            val colCount = data.csvCols(selectedFile) - 1
            if (colCount != -2) {
              val dialog = new TextInputDialog(defaultValue = "0, 1") {
                initOwner(stage)
                title = "Confirm columns"
                contentText = "Separate the columns that you want with a comma! Your file contains columns from 0 to " + colCount + "."
                headerText = "Choose the columns of your data file."
              }
              val result: Seq[String] = dialog.showAndWait().get.split(',')
              if (dialog.showAndWait().isDefined) {
              if (result.length == 2) {
                if (isInteger(result(0).trim) && isInteger(result(1).trim) && result(0).trim.toInt >= 0 && result(0).trim.toInt <= colCount && result(1).trim.toInt >= 0 && result(1).trim.toInt <= colCount) {
                  data.readCSV(selectedFile, (result(0).trim.toInt, result(1).trim.toInt))
                  if (!data.getData.get.isEmpty && fileNotIncluded(selectedFile.getName.mkString + result(0).trim + result(1).trim)) {
                    var regression = Exponential(data.getData.get, selectedFile.getName.mkString + result(0).trim + result(1).trim)
                    regressionModels = regressionModels :+ regression
                    list.getItems.add(selectedFile.getName.mkString + result(0).trim + result(1).trim)
                  } else {
                    val alert = new Alert(AlertType.Error) {
                      initOwner(stage)
                      title = "Error"
                      headerText = "The columns you chose did not contain doubles or this file already exists!"
                    }.showAndWait()
                  }
                } else {
                  val alert = new Alert(AlertType.Error) {
                    initOwner(stage)
                    title = "Error"
                    headerText = "You did not choose correct type of columns!"
                  }.showAndWait()
                }
              } else {
                val alert = new Alert(AlertType.Error) {
                  initOwner(stage)
                  title = "Error"
                  headerText = "You did not choose correct type of columns!"
                }.showAndWait()
              }
            }
          } else {
              val alert = new Alert(AlertType.Error) {
                initOwner(stage)
                title = "Error"
                headerText = "Your file was empty!"
              }.showAndWait()
            }
          }
          if (fileType != "csv" && fileType != "xlsx") {
            list.getItems.add(selectedFile.getName.mkString)
          }
        }  else {
          val alert = new Alert(AlertType.Error) {
            initOwner(stage)
            title = "Error"
            headerText = "This program only supports .txt, .xlsx and .csv files!"
          }.showAndWait()
        }
      } else {
        val alert = new Alert(AlertType.Error) {
          initOwner(stage)
          title = "Error"
          headerText = "This file already exists!"
        }.showAndWait()
      }
    }
  }

  /**
    * The logimport handles the logarithmic model import. It works exactly the same way as the linear import.
    */
  logImport.onAction = (e:ActionEvent) => {
    val fileChooser = new FileChooser()
    val selectedFile = fileChooser.showOpenDialog(stage)
    if (selectedFile != null) {
      if (fileNotIncluded(selectedFile.getName.mkString)) {
        val fileType = selectedFile.getName.mkString.split('.').last
        if (fileType == "txt" || fileType == "xlsx" || fileType == "csv") {
          if (fileType == "txt") {
            val data = new Data
            val readData = data.readTxt(selectedFile)
            var regression = Logarithmic(data.getData.get, selectedFile.getName.mkString)
            regressionModels = regressionModels :+ regression
          }
          if (fileType == "xlsx") {
            val data = new Data
            val colCount = data.excelCols(selectedFile) - 1
            if (colCount != -2) {
              val dialog = new TextInputDialog(defaultValue = "0, 1") {
                initOwner(stage)
                title = "Confirm columns"
                contentText = "Separate the columns that you want with a comma! Your file contains columns from 0 to " + colCount + "."
                headerText = "Choose the columns of your data file."

              }
              val result: Seq[String] = dialog.showAndWait().get.split(',')
              if (dialog.showAndWait().isDefined) {
              if (result.length == 2) {
                if (isInteger(result(0).trim) && isInteger(result(1).trim) && result(0).trim.toInt >= 0 && result(0).trim.toInt <= colCount && result(1).trim.toInt >= 0 && result(1).trim.toInt <= colCount) {
                  data.readExcelFile(selectedFile, (result(0).trim.toInt, result(1).trim.toInt))
                  if (!data.getData.get.isEmpty && fileNotIncluded(selectedFile.getName.mkString + result(0).trim + result(1).trim)) {
                    var regression = Logarithmic(data.getData.get, selectedFile.getName.mkString + result(0).trim + result(1).trim)
                    regressionModels = regressionModels :+ regression
                    list.getItems.add(selectedFile.getName.mkString + result(0).trim + result(1).trim)
                  } else {
                    val alert = new Alert(AlertType.Error) {
                      initOwner(stage)
                      title = "Error"
                      headerText = "The columns you chose did not contain doubles or this file already exists!"
                    }.showAndWait()
                  }
                } else {
                  val alert = new Alert(AlertType.Error) {
                    initOwner(stage)
                    title = "Error"
                    headerText = "You did not choose correct type of columns!"
                  }.showAndWait()
                }
              } else {
                val alert = new Alert(AlertType.Error) {
                  initOwner(stage)
                  title = "Error"
                  headerText = "You did not choose correct type of columns!"
                }.showAndWait()
              }
            }
          } else {
            val alert = new Alert(AlertType.Error) {
              initOwner(stage)
              title = "Error"
              headerText = "Your file was empty!"
            }.showAndWait()
          }
          }
          if (fileType == "csv") {
            val data = new Data
            val colCount = data.csvCols(selectedFile) - 1
            if (colCount != -2) {
              val dialog = new TextInputDialog(defaultValue = "0, 1") {
                initOwner(stage)
                title = "Confirm columns"
                contentText = "Separate the columns that you want with a comma! Your file contains columns from 0 to " + colCount + "."
                headerText = "Choose the columns of your data file."
              }
              val result: Seq[String] = dialog.showAndWait().get.split(',')
              if (dialog.showAndWait().isDefined) {
              if (result.length == 2) {
                if (isInteger(result(0).trim) && isInteger(result(1).trim) && result(0).trim.toInt >= 0 && result(0).trim.toInt <= colCount && result(1).trim.toInt >= 0 && result(1).trim.toInt <= colCount) {
                  data.readCSV(selectedFile, (result(0).trim.toInt, result(1).trim.toInt))
                  if (!data.getData.get.isEmpty && fileNotIncluded(selectedFile.getName.mkString + result(0).trim + result(1).trim)) {
                    var regression = Logarithmic(data.getData.get, selectedFile.getName.mkString + result(0).trim + result(1).trim)
                    regressionModels = regressionModels :+ regression
                    list.getItems.add(selectedFile.getName.mkString + result(0).trim + result(1).trim)
                  } else {
                    val alert = new Alert(AlertType.Error) {
                      initOwner(stage)
                      title = "Error"
                      headerText = "The columns you chose did not contain doubles or this file already exists!"
                    }.showAndWait()
                  }
                } else {
                  val alert = new Alert(AlertType.Error) {
                    initOwner(stage)
                    title = "Error"
                    headerText = "You did not choose correct type of columns!"
                  }.showAndWait()
                }
              } else {
                val alert = new Alert(AlertType.Error) {
                  initOwner(stage)
                  title = "Error"
                  headerText = "You did not choose correct type of columns!"
                }.showAndWait()
              }
            }
          } else {
            val alert = new Alert(AlertType.Error) {
              initOwner(stage)
              title = "Error"
              headerText = "Your file was empty!"
            }.showAndWait()
          }
          }
          if (fileType != "csv" && fileType != "xlsx") {
            list.getItems.add(selectedFile.getName.mkString)
          }
        }  else {
          val alert = new Alert(AlertType.Error) {
            initOwner(stage)
            title = "Error"
            headerText = "This program only supports .txt, .xlsx and .csv files!"
          }.showAndWait()
        }
      } else {
          val alert = new Alert(AlertType.Error) {
            initOwner(stage)
            title = "Error"
            headerText = "This file already exists!"
          }.showAndWait()
        }
    }
  }

  /**
    * Plotting all of the imported files.
    */
  buttonStart.onAction = (e:ActionEvent) => {
    val emptyFields: Boolean = textXs.getText.trim.isEmpty && textXb.getText.trim.isEmpty && textYs.getText.trim.isEmpty && textYb.getText.trim.isEmpty
    if(emptyFields && regressionModels.nonEmpty) {
      buttonClicked = true
      if (checkBoxGridOn.selected()) {
        gridOn = true
      }
      if (checkBoxLegend.selected() && !legendText.getText.isEmpty) {
        legendBoolean = true
      }

      val data = new Data
      data.changeData(regressionModels(0).seq)
      var xSmall = data.smallest.get._1
      var xBig = data.biggest.get._1
      var ySmall = data.smallest.get._2
      var yBig = data.biggest.get._2
      for (reg <- regressionModels) {
        data.changeData(reg.seq)
        if (data.smallest.get._1 < xSmall) {
          xSmall = data.smallest.get._1
        }
        if (data.smallest.get._2 < ySmall) {
          ySmall = data.smallest.get._2
        }
        if (data.biggest.get._1 > xBig) {
          xBig = data.biggest.get._1
        }
        if (data.biggest.get._2 > yBig) {
          yBig = data.biggest.get._2
        }

      }

      val legendSeq = legendText.getText.replaceAll(", ", ",").trim.split(",")
      if (!comboBox.getSelectionModel.isEmpty) {
        dataSize = comboBox.getSelectionModel.getSelectedItem.toString.toInt
      }
      if (!comboBoxLine.getSelectionModel.isEmpty) {
        lineWidth = comboBoxLine.getSelectionModel.getSelectedItem.toString.toInt
      }
      if (!plotSize.getSelectionModel.isEmpty) {
        plotSizeValue = plotSize.getSelectionModel.getSelectedItem.toString
      }
      if (plotSizeValue == "Small") {
        stageWidth = 700
        stageHeight = 600
      }
      if (plotSizeValue == "Medium") {
        stageWidth = 910
        stageHeight = 780
      }
      if (plotSizeValue == "Large") {
        stageWidth = 1140
        stageHeight = 820
      }
      var plot = new Plot((stageWidth - 160).toInt, ((stageWidth - 160) * 1.0 / stageWidth * stageHeight).toInt, gridOn, regressionModels, (xSmall, xBig), (ySmall, yBig), false, titleText.getText, xlabelField.getText, ylabelField.getText, legendBoolean, "nw", legendSeq, dataSize, lineWidth)
      if (regressionModels.length <= 5) {
          var plotScene: Scene = plot.plot
          stage.width = stageWidth
          stage.height = stageHeight
          stage.setScene(plotScene)
          stage.show()
          stage.title = plot.getTitle
          legendBoolean = false
          gridOn = false
      } else {
        val alert = new Alert(AlertType.Error) {
          initOwner(stage)
          title = "Error"
          headerText = "Too many models!"
          contentText = "You can only plot five models at a time."
        }.showAndWait()
      }

    }
    else if ((isDouble(textXs.getText) && isDouble(textXb.getText) && isDouble(textYs.getText) && isDouble(textYb.getText)) && regressionModels.nonEmpty && (textXs.getText.toDouble < textXb.getText.toDouble) && (textYs.getText.toDouble < textYb.getText.toDouble)) {
      buttonClicked = true
      if (checkBoxGridOn.selected()) {
        gridOn = true
      }
      if (checkBoxLegend.selected() && !legendText.getText.isEmpty) {
        legendBoolean = true
      }


      val xSmall = textXs.getText.toDouble
      val xBig = textXb.getText.toDouble
      val ySmall = textYs.getText.toDouble
      val yBig = textYb.getText.toDouble




      val legendSeq = legendText.getText.replaceAll(", ", ",").trim.split(",")
      if (!comboBox.getSelectionModel.isEmpty) {
        dataSize = comboBox.getSelectionModel.getSelectedItem.toString.toInt
      }
      if (!comboBoxLine.getSelectionModel.isEmpty) {
        lineWidth = comboBoxLine.getSelectionModel.getSelectedItem.toString.toInt
      }
      if (!plotSize.getSelectionModel.isEmpty) {
        plotSizeValue = plotSize.getSelectionModel.getSelectedItem.toString
      }
      if (plotSizeValue == "Small") {
        stageWidth = 700
        stageHeight = 600
      }
      if (plotSizeValue == "Medium") {
        stageWidth = 910
        stageHeight = 780
      }
      if (plotSizeValue == "Large") {
        stageWidth = 1140
        stageHeight = 820
      }
      var plot = new Plot((stageWidth - 160).toInt, ((stageWidth - 160) * 1.0 / stageWidth * stageHeight).toInt, gridOn, regressionModels, (xSmall, xBig), (ySmall, yBig), false, titleText.getText, xlabelField.getText, ylabelField.getText, legendBoolean, "nw", legendSeq, dataSize, lineWidth)
      if (regressionModels.length <= 5) {
        if (plot.multiplierX * (textXb.getText.toDouble - textXs.getText.toDouble) >= 5 && plot.multiplierY * (textYb.getText.toDouble - textYs.getText.toDouble) >= 5) {
          var plotScene: Scene = plot.plot
          stage.width = stageWidth
          stage.height = stageHeight
          stage.setScene(plotScene)
          stage.show()
          stage.title = plot.getTitle
          legendBoolean = false
          gridOn = false
        } else {
          val alert = new Alert(AlertType.Error) {
            initOwner(stage)
            title = "Error"
            headerText = "The range of the axes is too small or you have too many models!"
            contentText = "The range of your x axis should be at least: " + 5 / plot.multiplierX + ".\n" + "The range of your y axis should be at least: " + 5 / plot.multiplierY + "."
          }.showAndWait()
        }
      } else {
        val alert = new Alert(AlertType.Error) {
          initOwner(stage)
          title = "Error"
          headerText = "Too many models!"
          contentText = "You can only plot five models at a time."
        }.showAndWait()
      }
    } else {
      val alert = new Alert(AlertType.Error) {
        initOwner(stage)
        title = "Error"
        headerText = "Check the format of the input!"
        contentText = "You either have no files selected or the input of the axes is wrong. Please notice that you have to fill the fields that have the * sign. You have to give an input where the starting value is smaller than the ending value. The input field only accepts doubles."
      }.showAndWait()
    }
  }
  /**
    * Plotting selected files.
    */
  chosenStart.onAction = (e:ActionEvent) => {
    val selectedItems = list.getSelectionModel.getSelectedItems
    val sequence: Seq[Regression] = {
      for (regressionModel <- regressionModels; selectedItem <- selectedItems; if regressionModel.getTitle == selectedItem)
        yield regressionModel
    }
    val emptyFields: Boolean = textXs.getText.trim.isEmpty && textXb.getText.trim.isEmpty && textYs.getText.trim.isEmpty && textYb.getText.trim.isEmpty
    if(emptyFields && sequence.nonEmpty) {
      buttonClicked = true
      if (checkBoxGridOn.selected()) {
        gridOn = true
      }
      if (checkBoxLegend.selected() && !legendText.getText.isEmpty) {
        legendBoolean = true
      }
      val data = new Data
      data.changeData(sequence(0).seq)
      var xSmall = data.smallest.get._1
      var xBig = data.biggest.get._1
      var ySmall = data.smallest.get._2
      var yBig = data.biggest.get._2
      for (reg <- sequence) {
        data.changeData(reg.seq)
        if (data.smallest.get._1 < xSmall) {
          xSmall = data.smallest.get._1
        }
        if (data.smallest.get._2 < ySmall) {
          ySmall = data.smallest.get._2
        }
        if (data.biggest.get._1 > xBig) {
          xBig = data.biggest.get._1
        }
        if (data.biggest.get._2 > yBig) {
          yBig = data.biggest.get._2
        }

      }

      val legendSeq = legendText.getText.replaceAll(", ", ",").trim.split(",")
      if (!comboBox.getSelectionModel.isEmpty) {
        dataSize = comboBox.getSelectionModel.getSelectedItem.toString.toInt
      }
      if (!comboBoxLine.getSelectionModel.isEmpty) {
        lineWidth = comboBoxLine.getSelectionModel.getSelectedItem.toString.toInt
      }
      if (!plotSize.getSelectionModel.isEmpty) {
        plotSizeValue = plotSize.getSelectionModel.getSelectedItem.toString
      }
      if (plotSizeValue == "Small") {
        stageWidth = 700
        stageHeight = 600
      }
      if (plotSizeValue == "Medium") {
        stageWidth = 910
        stageHeight = 780
      }
      if (plotSizeValue == "Large") {
        stageWidth = 1140
        stageHeight = 820
      }
      var plot = new Plot((stageWidth - 160).toInt, ((stageWidth - 160) * 1.0 / stageWidth * stageHeight).toInt, gridOn, sequence, (xSmall, xBig), (ySmall, yBig), false, titleText.getText, xlabelField.getText, ylabelField.getText, legendBoolean, "nw", legendSeq, dataSize, lineWidth)
      if (sequence.length <= 5) {
        var plotScene: Scene = plot.plot
        stage.width = stageWidth
        stage.height = stageHeight
        stage.setScene(plotScene)
        stage.show()
        stage.title = plot.getTitle
        legendBoolean = false
        gridOn = false
      } else {
        val alert = new Alert(AlertType.Error) {
          initOwner(stage)
          title = "Error"
          headerText = "Too many models!"
          contentText = "You can only plot five models at a time."
        }.showAndWait()
      }

    }
    else if ((isDouble(textXs.getText) && isDouble(textXb.getText) && isDouble(textYs.getText) && isDouble(textYb.getText)) && sequence.nonEmpty && (textXs.getText.toDouble < textXb.getText.toDouble) && (textYs.getText.toDouble < textYb.getText.toDouble)) {
      if (checkBoxGridOn.selected()) {
        gridOn = true
      }
      if (checkBoxLegend.selected() && !legendText.getText.isEmpty) {
        legendBoolean = true
      }
      if (!plotSize.getSelectionModel.isEmpty) {
        plotSizeValue = plotSize.getSelectionModel.getSelectedItem.toString
      }
      var xSmall = textXs.getText.toDouble
      var xBig = textXb.getText.toDouble
      var ySmall = textYs.getText.toDouble
      var yBig = textYb.getText.toDouble
      var legendSeq = legendText.getText.replaceAll(", ", ",").trim.split(",")
      if (!comboBox.getSelectionModel.isEmpty) {
        dataSize = comboBox.getSelectionModel.getSelectedItem.toString.toInt
      }
      if (!comboBoxLine.getSelectionModel.isEmpty) {
        lineWidth = comboBoxLine.getSelectionModel.getSelectedItem.toString.toInt
      }
      if (plotSizeValue == "Small") {
        stageWidth = 700
        stageHeight = 600
      }
      if (plotSizeValue == "Medium") {
        stageWidth = 910
        stageHeight = 780
      }
      if (plotSizeValue == "Large") {
        stageWidth = 1140
        stageHeight = 820
      }

      var plot = new Plot((stageWidth - 160).toInt, ((stageWidth - 160) * 1.0 / stageWidth * stageHeight).toInt, gridOn, sequence, (xSmall, xBig), (ySmall, yBig), false, titleText.getText, xlabelField.getText, ylabelField.getText, legendBoolean, "nw", legendSeq, dataSize, lineWidth)
      if (sequence.length <= 5) {
        if (plot.multiplierX * (textXb.getText.toDouble - textXs.getText.toDouble) >= 5 && plot.multiplierY * (textYb.getText.toDouble - textYs.getText.toDouble) >= 5) {
          var plotScene: Scene = plot.plot
          stage.width = stageWidth
          stage.height = stageHeight
          stage.setScene(plotScene)
          stage.show()
          stage.title = plot.getTitle
          legendBoolean = false
          gridOn = false
        } else {
          val alert = new Alert(AlertType.Error) {
            initOwner(stage)
            title = "Error"
            headerText = "The range of the axes is too small!"
            contentText = "The range of your x axis should be at least: " + 5 / plot.multiplierX + ".\n" + "The range of your y axis should be at least: " + 5 / plot.multiplierY + "."
          }.showAndWait()
        }
      } else {
        val alert = new Alert(AlertType.Error) {
          initOwner(stage)
          title = "Error"
          headerText = "Too many models!"
          contentText = "You can only plot five models at a time."
        }.showAndWait()
      }
    } else {
      val alert = new Alert(AlertType.Error) {
        initOwner(stage)
        title = "Error"
        headerText = "Check the format of the input!"
        contentText = "You either have no files selected or the input of the axes is wrong. Please notice that you have to fill the fields that have the * sign. You have to give an input where the starting value is smaller than the ending value. The input field only accepts doubles."
      }.showAndWait()
    }
  }

  remove.onAction = (e:ActionEvent) => { //Removing all files
    regressionModels = Seq[Regression]()
    list.getItems.clear()
  }

  val startScene = new Scene() { //The starting scene
    content = new Pane() {
      children = Seq(buttonStart,list,checkBoxLegend, checkBoxGridOn,textXs,textXb,textYs,textYb,ylabelField,xlabelField,legendText,comboBox,menuBar, textbx,textby,textsx,textsy,legendtxt,
        txtTitle,xlabelText,ylabelText, titleText,parametersText,adjustments,datapointText,dataFiles,plotSize, comboBoxLine, lineText, chosenStart, plotSizeText, text)
    }
  }

  /**
    * Defining the stage
    */
  stage = new JFXApp.PrimaryStage {
    width = stageWidth+30
    height = stageHeight-230

    scene = startScene

   }
  stage.setResizable(false) //The stage is not resizable.
}


