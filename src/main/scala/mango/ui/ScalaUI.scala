package mango.ui

import java.awt.{ Color, Dimension, Font }
import java.io.File
import scala.Array.canBuildFrom
import scala.swing.{ Alignment, BorderPanel }
import scala.swing.{ BoxPanel, Button, Label, ListView }
import scala.swing.{ MainFrame, MenuBar, Orientation, ScrollPane, Separator, SimpleSwingApplication, SplitPane, TabbedPane, Table }
import scala.swing.BorderPanel.Position.South
import scala.swing.ListView.IntervalMode
import scala.swing.event.{ ButtonClicked, ListSelectionChanged, WindowClosing }
import javax.imageio.ImageIO
import javax.swing.table.AbstractTableModel
import mango.analyser.{ ArrayField, BooleanField, CollectionMergedSchema, CollectionSchema, CouchDBParser, DBAnalyser, DateField, DoubleField, Field, IntegerField, LongField, MongoDBParser, NullField, ObjectField, ObjectIdField, Parser, StringField }
import mango.analyser.RethinkDBParser

object Config {
  val dbs = Seq("MongoDB", "RethinkDB", "CouchDB")
  val host = "localhost"
  val port = 27017
}

case class Config(docCount: Option[Int], selectedDB: String, host: String, port: Int) {
  def makeParser: Parser = {
    if (selectedDB equalsIgnoreCase "CouchDB") {
      new CouchDBParser(this)
    } else if (selectedDB equalsIgnoreCase "MongoDB") {
      new MongoDBParser(host, port)
    } else if (selectedDB equalsIgnoreCase "RethinkDB") {
      new RethinkDBParser(host, port)
    }else {
      null
    }
  }
}

object UIState {
  var dbName: String = _
  var collectionNames: List[String] = _
  var collNames: List[String] = _
}

object ScalaUI extends SimpleSwingApplication {

  private var config = Config(None, Config.dbs.head, Config.host, Config.port)
  private val settingDialog = new SettingsDialog(config = _)

  private val dbAnalyser = new DBAnalyser

  private val mainContents: ScrollPane = new ScrollPane {
    visible = false

    reactions += {
      case WindowClosing(_) =>
        sys.exit(0)
    }
  }

  private val dbView = new ScrollPane with Padding with GroupedPanel {
    setGroupTitle("Databases")
  }

  private val statusLabel = new Label("Main Panel") with labelSetting {
    horizontalAlignment = Alignment.Left
  }

  private val classD = new ClassDiagramPanel with Padding with panelSetting

  private val getRelationsButton = new Button("Get Relations") with buttonSetting {
    visible = false
    reactions += {
      case ButtonClicked(b) => {
        statusLabel.text = "Analysing Relationship"
        classD.drawRelation(config)
      }
    }
  }

  private val collectionView = new ScrollPane with Padding with GroupedPanel {
    val list: ListView[String] = new ListView[String] with nameListSetting {
      listenTo(this.selection)
      reactions += {
        case l: ListSelectionChanged[String] => {
          if (!l.live) {
            getRelationsButton.visible = list.selection.items.length >= 2

            statusLabel.text = "Collection View"

            UIState.collectionNames = selection.items.toList
            UIState.collNames = selection.items.toList
            classD.onCollectionsSelect(config)
            val tabbedpane = new TabbedPane
            UIState.collectionNames.foreach(coll => {
              val dbParser = config.makeParser

              val rowFields = dbParser.parseCollection(UIState.dbName, coll).toArray
              val mergedSchema = dbAnalyser.analyse(Seq(CollectionSchema(coll, rowFields.toList)))
              val names = getNames(rowFields)
              val columnHeaders = getColoumnHeader(mergedSchema, names.toSeq)
              val spreadSheet = new Spreadsheet(rowFields, columnHeaders, names.toSeq.size)
              tabbedpane.pages += new TabbedPane.Page(coll, spreadSheet)

            })
            collectionSchema.tab.contents = tabbedpane
          }
        }
      }
    }
    contents = list
    setGroupTitle("Collections")
  }

  private def getNames(collFields: Array[Seq[Field]]) = {
    collFields.map(row => {
      row.map(field => field.name)
    }).flatten.toSet
  }

  def getColoumnHeader(mergedSchemas: Seq[CollectionMergedSchema], coloumnNames: Seq[String]) = {
    coloumnNames.map(name => {
      mergedSchemas.map(schema => {
        schema.fields.filter(_.name == name)
      }).flatten.maxBy(_.count)
    })
  }

  class Spreadsheet(rows: Array[Seq[Field]], columnHeaders: Seq[Field], val width: Int) extends ScrollPane {
    private val rowData = {
      val allColNames = getNames(rows).toSeq
      val filteredRows = config.docCount.map(rows.take(_)).getOrElse(rows)
      val colMap = filteredRows map { row =>
        row.map(field => field.name -> field).toMap
      }
      colMap.map(row => {
        allColNames.map(name => row.get(name).map(getCellValue(_, columnHeaders)).getOrElse(" ! ")).toArray[Any]
      })
    }

    private val table = new Table(rowData, columnHeaders) with panelSetting {
      model = new AbstractTableModel {
        override def getColumnName(column: Int) = columnHeaders(column).toString
        def getRowCount() = rowData.length
        def getColumnCount() = columnHeaders.length
        def getValueAt(row: Int, col: Int): AnyRef = rowData(row)(col).asInstanceOf[AnyRef]
        override def isCellEditable(row: Int, column: Int) = false
        override def setValueAt(value: Any, row: Int, col: Int) {
          rowData(row)(col) = value
          fireTableCellUpdated(row, col)
        }
      }
      gridColor = new Color(150, 150, 150)
    }
    viewportView = table
    rowHeaderView =
      new ListView((0 until rowData.length) map (_.toString)) {
        fixedCellWidth = 30
        fixedCellHeight = table.rowHeight
      }
  }

  private val collectionSchema = new BoxPanel(Orientation.Vertical) with panelSetting {
    val tab = new ScrollPane with panelSetting
    contents += tab
  }


  private def getCellValue(field: Field, columnHeaders: Seq[Field]) = {
    if (columnHeaders.exists(header => dbAnalyser.isFieldSimilar(header, field))) getValue(field)
    else getValueWithType(field)
  }

  private def getValue(field: Field) = {
    field match {
      case id: ObjectIdField => id.value.toString
      case d: DoubleField => d.value.toString
      case i: IntegerField => i.value.toString
      case l: LongField => l.value.toString
      case date: DateField => date.value.toString
      case s: StringField => s.value.toString
      case o: ObjectField => o.value.toString
      case a: ArrayField => a.value.toString
      case b: BooleanField => b.value.toString
      case n: NullField => " "
    }
  }

  private def getValueWithType(field: Field) = {
    field match {
      case id: ObjectIdField => id.value + " : " + id.fieldType
      case d: DoubleField => d.value + " : " + d.fieldType
      case i: IntegerField => i.value + " : " + i.fieldType
      case l: LongField => l.value + " : " + l.fieldType
      case date: DateField => date.value + " : " + date.fieldType
      case s: StringField => s.value + " : " + s.fieldType
      case o: ObjectField => o.value + " : " + o.fieldType
      case a: ArrayField => a.value + " : " + a.fieldType
      case b: BooleanField => b.value + " : " + b.fieldType
      case n: NullField => " "
    }
  }

  private def mkMain = {

    val statusBar = new BorderPanel {
      font = new Font("SansSerif", Font.PLAIN, 14)
      maximumSize = new Dimension(toolkit.getScreenSize.getWidth.toInt, 20)
      add(statusLabel, South)
    }

      def getMergedFieldNames(mergedSchemas: Seq[CollectionMergedSchema]) = {
        mergedSchemas.flatMap(schema => schema.fields.map(field => field.name))
      }

      def getColoumnValues(collFields: Array[Seq[Field]]) = {
        val coloumnValues = collFields.map(row => {
          row.maxBy(field => field.count)
        })
        coloumnValues.map(coloumn => coloumn.name + ": " + coloumn.fieldType)
      }

    val relationPanel = new BoxPanel(Orientation.Vertical) with panelSetting {

      contents += new MenuBar {
        contents += getRelationsButton
      }
      contents += classD
      minimumSize = new Dimension(400, 180)
      preferredSize = new Dimension(400, classD.bounds.height.toInt)
    }

    val rightPanel = new SplitPane(Orientation.Horizontal, relationPanel, collectionSchema) with Padding with panelSetting {
      preferredSize = new Dimension(400, 400)
    }

    val mainPanel: BoxPanel = new BoxPanel(Orientation.Horizontal) {
      private val leftPanel = new BoxPanel(Orientation.Vertical) with Padding with panelSetting {
        maximumSize = new Dimension(300, toolkit.getScreenSize.getHeight.toInt)

        contents += dbView
        contents += new Separator with Padding
        contents += collectionView
      }

      contents += new SplitPane(Orientation.Vertical, leftPanel, rightPanel)
    }

    val mainPanelWithStatusBar = new BoxPanel(Orientation.Vertical) with Padding {
      contents += mainPanel
      contents += statusBar
    }

    mainPanelWithStatusBar
  }

  private val mainFrame = new MainFrame {
    val icon = ImageIO.read(new File("graphics/mangoLogo.png"))
    peer.setIconImage(icon)
    title = "Mango"
    contents = mainContents
  }

  settingDialog.open

  def begin = {
    println("Beginning")
    val parser = config.makeParser
    val dbNames = parser.getdbNames
    println("Db names", dbNames)

    dbView.contents = new ListView(dbNames) with nameListSetting {
      selection.intervalMode = IntervalMode.Single
      listenTo(this.selection)
      reactions += {
        case l: ListSelectionChanged[String] => {
          if (!l.live) {
            UIState.dbName = selection.items(0)

            val collNames = {
              val db = selection.items(0)
              parser.getCollectionNames(db.toString)
            }
            collectionView.list.listData = collNames
            classD.onDatabaseSelect
            collectionSchema.tab.contents = new ScrollPane
          }
        }
      }
    }

    mainContents.contents = mkMain
    mainContents.visible = true
    mainFrame.pack
    mainFrame.centerOnScreen
  }

  def top = mainFrame
}
