package mango.ui

import java.awt.Dimension
import scala.Option.option2Iterable
import scala.swing.Alignment
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.Component
import scala.swing.Label
import scala.swing.Orientation
import scala.swing.event.ButtonClicked
import com.mxgraph.layout.mxStackLayout
import com.mxgraph.model.mxCell
import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.view.mxGraph
import javax.swing.JPopupMenu
import mango.analyser.ArrayField
import mango.analyser.BooleanField
import mango.analyser.CollectionSchema
import mango.analyser.DBAnalyser
import mango.analyser.DateField
import mango.analyser.DoubleField
import mango.analyser.Field
import mango.analyser.IntegerField
import mango.analyser.LongField
import mango.analyser.NullField
import mango.analyser.ObjectField
import mango.analyser.ObjectIdField
import mango.analyser.StringField
import mango.relation.FieldRef
import mango.relation.Relation
import mango.relation.RelationAnalyser
import com.mxgraph.view.mxCellState
import com.mxgraph.util.mxPoint
import scala.collection.JavaConverters._
import com.mxgraph.util.mxRectangle

final case class Point[T <% Ordered[T]](x: T, y: T) {
  def getLower[X](that: Point[T]) = {
    val lowerX = min(x, that.x)
    val lowerY = min(y, that.y)
    Point(lowerX, lowerY)
  }

  def getHigher(that: Point[T]) = {
    val higherX = max(x, that.x)
    val higherY = max(y, that.y)
    Point(higherX, higherY)
  }

  private def min(a: T, b: T) = if (a < b) a else b
  private def max(a: T, b: T) = if (a > b) a else b

  def map[U <% Ordered[U]](f: (T) => U) = Point(f(x), f(y))

  override def toString = "Pt(" + x + "," + y + ")"
}

final case class VertexRelation(collectionName: String, vertices: Seq[Object]) {
  override def toString = {
    collectionName + " => " + vertices
  }
}

class ClassDiagramPanel extends BoxPanel(Orientation.Vertical) {
  private def getSchema(field: Field) = {
    field match {
      case id: ObjectIdField => id.nameIn + " : " + id.fieldType + "(" + id.count + ")"
      case d: DoubleField => d.nameIn + " : " + d.fieldType + "(" + d.count + ")"
      case i: IntegerField => i.nameIn + " : " + i.fieldType + "(" + i.count + ")"
      case l: LongField => l.nameIn + " : " + l.fieldType + "(" + l.count + ")"
      case date: DateField => date.nameIn + " : " + date.fieldType + "(" + date.count + ")"
      case s: StringField => s.nameIn + " : " + s.fieldType + "(" + s.count + ")"
      case o: ObjectField => o.nameO + " : " + o.fieldType + "(" + o.count + ")"
      case a: ArrayField => a.nameIn + " : " + a.fieldType + "(" + a.count + ")"
      case b: BooleanField => b.nameIn + " : " + b.fieldType + "(" + b.count + ")"
      case n: NullField => " "
    }
  }

  private def getFieldId(collectionName:String, field: Field) = {
    collectionName + ":" + field.name + ":" + field.fieldType
  }

  private def getCell(fieldRef: FieldRef, vertexRelations: List[VertexRelation]) = {
    val id = getFieldId(fieldRef.collectionName, fieldRef.field)
    val res = vertexRelations.filter(_.collectionName == fieldRef.collectionName).flatMap {vr =>
      vr.vertices.filter {
        case cell: mxCell => cell.getId == id
        case _ => false
      }
    }.headOption
    res
  }

  private def createGraph(graphPopulater: mxGraph => Unit) = {
    contents.clear
    val graph = new mxGraph
    val style = graph.getStylesheet.getDefaultEdgeStyle //.put(mxConstants., mxEdgeStyle.EntityRelation)
    graph.getModel.beginUpdate()
    graphPopulater(graph)
    graph.getModel.endUpdate()

    val layout = new mxStackLayout(graph, true, 30)
    layout.execute(graph.getDefaultParent)

    routeEdgePoints(graph)

    val graphComponent = new mxGraphComponent(graph)
    graphComponent.setEnabled(false)
    contents += Component.wrap(graphComponent)
    revalidate

  }

  private def routeEdgePoints(graph: mxGraph) = {
    val cellStates = graph.getView().getStates.values.asScala.filter(_ != null).toList
    val (bounds, costMap) = findCostMap(cellStates)
    val connections = cellStates.filter(_.getAbsolutePoints != null)
    connections.zipWithIndex.foreach { case (c,i) =>
      // c.setAbsolutePoints()
      val points = c.getAbsolutePoints.asScala.filter(_ != null)
      val startPoint = Point(points.head.getX, points.head.getY)
      val endPoint = Point(points.last.getX, points.last.getY)
      println("Routing", startPoint, endPoint)
      val startTime = System.currentTimeMillis()
      val aStar = new Astar(startPoint.map(_.toInt), endPoint.map(_.toInt), costMap)
      val route = aStar.execute(bounds)
      val endTime = System.currentTimeMillis()
      println("Time taken = " + (endTime - startTime))
      println("\nThe newer points are " + route)
      c.setAbsolutePoints(route.map(p => new mxPoint(p.x, p.y)).asJava)
      // DebugHelper.costToImg(costMap, "costMap%d.png" format (i), route)
    }
  }

  private def makeClassNode(collectionName: String, graph: mxGraph, config: Config) = {
    val dbParser = config.makeParser
    val dbAnalyser = new DBAnalyser
    val schema = dbParser.parseCollection(UIState.dbName, collectionName).toList
    val mergedSchemas = dbAnalyser.analyse(Seq(CollectionSchema(collectionName, schema.toList)))
    val fieldValue = mergedSchemas.map(mergedSchema => mergedSchema.fields).flatten.toSet

    val defParent = graph.getDefaultParent

    val parent = graph.insertVertex(defParent, null, collectionName, 20, 20, 80, 24, "fillColor=#AAAAFF;fontSize=22;fontStyle=1;fontFamily=Ubuntu, Arial, Helvetica, sans-serif;fontColor=#000000")
    var vertices = List[AnyRef]()
    fieldValue foreach { value =>
      val id = getFieldId(collectionName, value)
      val vertex = graph.insertVertex(parent, id, getSchema(value), 0, 0, 80, 24, "fillColor=#AAAAAA;fontSize=18;fontFamily=Ubuntu, Arial, Helvetica, sans-serif;portConstraint=eastwest;fontColor=#000000")
      vertices :+= vertex
    }

    val allCells = parent :: vertices
    val layout = new mxStackLayout(graph, false)
    allCells.foreach(graph.updateCellSize)
    allCells.foreach(layout.execute)

    VertexRelation(collectionName, vertices)
  }

  private def getEdges(vertexRelations: List[VertexRelation], relations: Set[Relation]) = {
    relations.flatMap(relation => {
      val from = getCell(relation.association.field1, vertexRelations)
      val to = getCell(relation.association.field2, vertexRelations)
      if (from.isDefined && to.isDefined) {
        Some(from.get, to.get, relation.confidence)
      } else {
        None
      }
    })
  }

  val colors = Array (
    "ff8AA63D",
    "108A3DA6",
    "903D8AA6"
  )
  private def getEdgeColor(i:Int) = {
    colors(i % colors.length)
  }

  def drawRelation(config: Config) = {
    createGraph { graph =>
      var vertexRelations = List[VertexRelation]()
      UIState.collectionNames.foreach { collectionName =>
        val vertexRelation = makeClassNode(collectionName, graph, config)
        vertexRelations :+= vertexRelation

      }
      val dbParser = config.makeParser

      val rel = new RelationAnalyser(dbParser, UIState.dbName)
      val relations = rel.getCollRelations(UIState.collectionNames)

      if (relations.size > 0) {
        val defParent = graph.getDefaultParent
        getEdges(vertexRelations, relations).zipWithIndex.foreach {case (edge, i) => 
          val confidence = edge._3
          val opacity = (confidence * 100).toInt
          graph.insertEdge(defParent, null, " ", edge._1, edge._2, s"edgeStyle=orthogonalEdgeStyle;strokeWidth=2;opacity=$opacity;strokeColor=#${getEdgeColor(i)};")
        }
      } else {
        val label = new Label("Relation Not Found") with labelSetting {
          horizontalAlignment = Alignment.Left
        }
        val popup = new PopupMenu

        val button = new Button("Close")
        listenTo(button)
        reactions += {
          case e: ButtonClicked => {
            popup.visible = false
            button.visible = false
          }
        }

        popup.add(label)
        popup.add(button)
        popup.setLocation(800, 300)
        popup.setVisible(true)
      }
    }

  }

  def onCollectionsSelect(config: Config) = {
    createGraph { graph =>
      UIState.collectionNames.foreach(makeClassNode(_, graph, config))
    }
  }

  def onDatabaseSelect = {
    contents.clear
    revalidate
  }

  private def ifContains(row: Int, col: Int, xPoints: Set[Int], yPoints: Set[Int]) = xPoints.contains(row) && yPoints.contains(col)

  private def feather(bounds: Seq[mxRectangle], featherAmt: Double) = {
    val featherBounds = bounds.map(new mxRectangle(_))
    featherBounds.foreach(_.grow(featherAmt))
    featherBounds
  }

  val routePadding = 20
  val routeBlockCost = 120
  val routeFeatherCost = routeBlockCost / 10

  private def findCostMap(cellStates: List[mxCellState]) = {
    val bounds = cellStates.flatMap { s =>
      s.getBoundingBox match {
        case null => None
        case b if (s.getAbsolutePointCount == 0) => Some(b)
        case _ => None
      }
    }
    val featherAmt = 4
    val grownBounds = feather(bounds, featherAmt * 2)

    val allBounds = new mxRectangle(0, 0, 0, 0)
    bounds.foreach { b => allBounds.add(b) }
    val graphHeight = allBounds.getHeight.toInt
    val graphWidth = allBounds.getWidth.toInt
    val graphOriginX = allBounds.getX.toInt
    val graphOriginY = allBounds.getY.toInt

    val costMap = (0 to graphHeight + routePadding).map(row => {
      (0 until graphWidth + routePadding).map(col => {
        // TODO: Optimization: Directly iterate on x & y
        val x = col - graphOriginX
        val y = row - graphOriginY
        if (bounds.exists(_.contains(x, y))) {
          routeBlockCost
        } else if (grownBounds.exists(_.contains(x, y))) {
          routeFeatherCost
        } else {
          0
        }
      }).toArray
    }).toArray

    (bounds, costMap)
  }
}

class PopupMenu extends Component {
  override lazy val peer: JPopupMenu = new JPopupMenu
  this.preferredSize = new Dimension(200, 80)
  def add(button: Button): Unit = { peer.add(button.peer) }
  def add(label: Label): Unit = { peer.add(label.peer) }
  def setVisible(visible: Boolean): Unit = { peer.setVisible(visible) }
  def setLocation(x: Int, y: Int): Unit = { peer.setLocation(x, y) }
}