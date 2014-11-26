package mango.ui

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import scala.collection.mutable.PriorityQueue
import scala.util.control.Breaks.{ break, breakable }
import javax.imageio.ImageIO
import com.mxgraph.util.mxRectangle
import com.mxgraph.util.mxUtils
import com.mxgraph.util.mxPoint
import scala.collection.mutable.ArrayBuffer
import gnu.trove.map.hash.TLongIntHashMap

final case class Node(p: Point[Int], var g: Int, h: Int, var Parent: Node) {
  def f = g + h
  /*
  override def toString = {
    "(" + p.x + "," + p.y + ")"
  }
  * 
  */
  override def equals(that: Any) = {
    that match {
      case Node(tp, _, _, _) => p equals tp
      case _ => false
    }
  }
}

final class SortedPoints {
  private val heap = ArrayBuffer.empty[Node]
  private val nodeMap = new TLongIntHashMap(1024, 1f, -1, -1)

  def enqueue(n: Node) = {
    // println("Enqueuing", n.p)
    heap += n
    nodeMap.put(asLong(n.p), heap.size - 1)
    pushUp(heap.size - 1)
    // isSane // TODO: Remove
  }

  def asLong(p: Point[Int]) = {
    (p.x.toLong << 32) | (p.y.toLong)
  }

  def dequeue = {
    if (heap.size > 0) {
      swap(0, heap.size - 1);
      val result = heap.remove(heap.size - 1)
      nodeMap.remove(asLong(result.p))
      pushDown(0)
      // println("Dequeuing", result)
      // isSane // TODO: Remove
      result
    } else {
      null
    }
  }

  def isEmpty = heap.size == 0

  def find(p: Point[Int]): Option[(Node, Int)] = {
    val key = asLong(p)
    if (nodeMap.containsKey(key)) {
      val i = nodeMap.get(asLong(p))
      (Some(heap(i) -> i))
    } else {
      None
    }
  }

  private def isSane = {
    assert(nodeMap.keys.length == heap.size, "Failed nodeMap length (%d) != Heap size (%d)" format (nodeMap.keys.length, heap.size))

    {
      var i = 1
      val N = heap.size
      while (i < N) {
        assert(isLesserOrEqual(parent(i), i), "Failed: %d <= %d (Index: %d, %d)" format (heap(parent(i)).f, heap(i).f, parent(i), i))
        i += 1
      }
    }
  }

  def recompute(index: Int) = {
    if (isLesserOrEqual(index, parent(index))) {
      pushUp(index)
    } else {
      pushDown(index)
    }
  }

  private def isLesserOrEqual(i: Int, j: Int) = {
    heap(i).f <= heap(j).f
  }

  private def pushUp(n: Int) = {
    var i = n
    while (i > 0 && !isLesserOrEqual(parent(i), i)) {
      swap(parent(i), i);
      i = parent(i);
    }
  }

  private def pushDown(i: Int): Unit = {
    val l = left(i)
    val r = right(i)
    var smallest = i

    if (l < heap.size && !isLesserOrEqual(smallest, l)) {
      smallest = l
    }
    if (r < heap.size && !isLesserOrEqual(smallest, r)) {
      smallest = r
    }

    if (smallest != i) {
      swap(smallest, i)
      pushDown(smallest)
    }
  }
  private def parent(i: Int) = (i - 1) / 2
  private def left(i: Int) = 2 * i + 1
  private def right(i: Int) = 2 * i + 2

  private def swap(i: Int, j: Int) = {
    val oldI = heap(i)
    val oldJ = heap(j)
    heap(i) = oldJ
    heap(j) = oldI
    nodeMap.put(asLong(oldI.p), j)
    nodeMap.put(asLong(oldJ.p), i)
  }
  override def toString = heap.mkString("\n")
}

final case class Astar(start: Point[Int], end: Point[Int], costMap: Array[Array[Int]]) {
  import Astar._

  val costWidth = costMap(0).length
  val costHeight = costMap.length
  // println(this)
  // println(costMap.map(_.mkString(", ")).mkString("\n"))

  val startNode = Node(start, 0, manhattanDist(start, end), null)
  val endNode = Node(end, manhattanDist(start, end), 0, null)

  // private var openQ = new PriorityQueue[Node]()
  private var openQ = new SortedPoints

  private var closedQ = Set[Point[Int]]()
  implicit def orderedNode(f: Node): Ordered[Node] = new Ordered[Node] {
    def compare(other: Node) = other.f.compare(f.f)
  }

  private val distScale = 10
  private val distScaleDiag = 14

  private def manhattanDist(currPoint: Point[Int], targetPoint: Point[Int]) = {
    distScale * (math.abs(currPoint.x - targetPoint.x) + math.abs(currPoint.y - targetPoint.y))
  }

  private def crowDist(currPoint: Point[Int], targetPoint: Point[Int]) = {
    val xD = math.abs(currPoint.x - targetPoint.x)
    val yD = math.abs(currPoint.y - targetPoint.y)
    val totalD = xD + yD
    math.min(totalD * distScale, distScaleDiag)
  }

  private def onClosedList(point: Point[Int]) = closedQ.contains(point)

  private def checkAndReplace(node: Node, currNode: Node) = {
    val existing = openQ.find(node.p)
    existing match {
      case Some((e, index)) =>
        if (node.g < e.g) {
          e.g = crowDist(node.p, currNode.p) + distScale * costMap(node.p.y)(node.p.x)
          node.Parent = currNode
          openQ.recompute(index)
          // println("In Check and Replaced with node  " + node)
        }
        true
      case _ => false
    }
  }

  private def addNode(x: Int, y: Int, currNode: Node, isdiag: Boolean) = {
    // TODO: isDiag can be auto computed
    val newPoint = Point(x, y)
    if (!onClosedList(newPoint)) {
      val g = currNode.g + {
        if (isdiag) distScaleDiag else distScale
      } + distScale * costMap(y)(x)
      val newNode = new Node(newPoint, g, manhattanDist(Point(x, y), endNode.p), currNode)
      // println("___________The NewNode is " + newNode)
      if (checkAndReplace(newNode, currNode) == false) {
        openQ.enqueue(newNode)
        // println("The Added Node IN OPENQ is " + newNode.p + "\n closed size:"+closedQ.length)
      }
    }
  }

  private def addNeighbours(node: Node, endNode: Node) = {
    val x = node.p.x
    val y = node.p.y

    if ((x + 1) < costWidth) addNode(x + 1, y, node, false)
    if ((x - 1) >= 0) addNode(x - 1, y, node, false)
    if ((y + 1) < costHeight) addNode(x, y + 1, node, false)
    if ((y - 1) > 0) addNode(x, y - 1, node, false)
    if ((y + 1) < costHeight && (x + 1) < costWidth) addNode(x + 1, y + 1, node, true)
    if ((y + 1) < costHeight && (x - 1) >= 0) addNode(x - 1, y + 1, node, true)
    if ((y - 1) > 0 && (x - 1) >= 0) addNode(x - 1, y - 1, node, true)
    if ((y - 1) >= 0 && (x + 1) < costWidth) addNode(x + 1, y - 1, node, true)
  }

  type Direction = (Int, Int)
  private def direction(p1: Node, p2: Node): Direction = {
    def boundByOne(x: Int) = math.min(1, math.max(-1, x))

    (boundByOne(p1.p.x - p2.p.x), boundByOne(p1.p.y - p2.p.y))
  }

  def execute(grownBounds: Seq[mxRectangle] = Nil) = {
    var temp: Option[Node] = None
    openQ.enqueue(startNode)
    breakable {
      while (!openQ.isEmpty) {
        val currNode = openQ.dequeue
        closedQ += currNode.p
        /*
        if (closedQ.size % 100 == 0) {
          println("curr Node is =" + currNode.p, "closed: " + closedQ.size)
        }
        * 
        */
        if (currNode.p equals endNode.p) {
          // println("&***********************************************&&&&&&&&&&&&&&&&&")
          // println("Closed size:", closedQ.size)
          temp = Some(currNode)
          break
        }
        addNeighbours(currNode, endNode)
      }
    }

    val nodes = if (temp.isDefined) {
      val pathPoints = Stream.iterate(temp.get)(x => x.Parent).takeWhile(_ != null).toList.reverse
      val initDirection = direction(pathPoints(0), pathPoints(1))
      // Delete points which are in the same direction as previous one, so that route is simplified.
      val pathCompressed = pathPoints.foldLeft[List[(Node, Direction)]](List(pathPoints(0) -> initDirection)) { (a, e) =>
        val newDir = direction(a.last._1, e)
        if (newDir == a.last._2) {
          a.dropRight(1) :+ (e, newDir)
        } else {
          a :+ (e, newDir)
        }
      }
      // Ignore the first point since it will be a duplicate
      pathCompressed.tail.map(_._1)
    } else {
      List(startNode, endNode)
    }
    val points = nodes.map(_.p)
    if (grownBounds.nonEmpty) {
      smoothen(grownBounds, points)
    } else {
      points
    }
  }

}

object Astar {

  def intersectLine(r: mxRectangle, p1: Point[Int], p2: Point[Int]) = {
    val x = r.getX
    val y = r.getY
    val width = r.getWidth
    val height = r.getHeight
    val xM = x + width
    val yM = y + height
    val i1 = mxUtils.intersection(x, y, xM, y, p1.x, p1.y, p2.x, p2.y)
    val i2 = mxUtils.intersection(xM, y, xM, yM, p1.x, p1.y, p2.x, p2.y)
    val i3 = mxUtils.intersection(xM, yM, x, yM, p1.x, p1.y, p2.x, p2.y)
    val i4 = mxUtils.intersection(x, y, x, yM, p1.x, p1.y, p2.x, p2.y)
    val intersections = List(i1, i2, i3, i4).filterNot(_ == null).map(p => Point(p.getX.toInt, p.getY.toInt)).filterNot(p => (p equals p1) || (p equals p2))
    /*
    println("\nrect: %f, %f, %f x %f" format (x, y, width, height))
    println(p1, p2)
    if (intersections.length > 0) {
      println("intersections: " + intersections.mkString(","))
    }
    */
    val uniqueIntersections = intersections.toSet
    uniqueIntersections.size > 0
  }
  def intersects(bounds: Seq[mxRectangle], p1: Point[Int], p2: Point[Int]) = bounds.exists(b => intersectLine(b, p1, p2))

  def smoothen(bounds: Seq[mxRectangle], points: Seq[Point[Int]]) = {
    val startPoint = points.head
    val startExcludedBounds = bounds.filterNot(_.contains(startPoint.x, startPoint.y))
    var built = List(startPoint)
    points.tail.sliding(2).foreach { pair =>
      val curr = pair(0)
      val next = pair(1)
      val pPrev = built.last
      val boundsToCheck = if (pPrev equals startPoint) startExcludedBounds else bounds
      val intersectsNext = intersects(boundsToCheck, pPrev, next)
      if (intersectsNext) {
        built :+= curr
      }
    }
    // println("Orig", points)
    // println("Smooth", built :+ points.last)
    built :+ points.last
  }
}

object DebugHelper {
  def costToImg(costMap: Array[Array[Int]], name: String, route: List[Point[Int]]) = {
    val width = costMap(0).length
    val height = costMap.length

    println(width, height)

    // TYPE_INT_ARGB specifies the image format: 8-bit RGBA packed
    // into integer pixels
    val bi = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)

    for (i <- 0 until width; j <- 0 until height) {
      val y = costMap(j)(i) & 0xff
      val c = (0xff << 24) | (y << 16) | (y << 8) | y
      bi.setRGB(i, j, c)
    }
    val g = bi.createGraphics
    def drawCircle(cx: Int, cy: Int, r: Int) = {
      g.fillOval(cx - r / 2, cy - r / 2, r, r)
    }
    val intermediates = route.drop(1).dropRight(1)
    g.setColor(Color.LIGHT_GRAY)
    route.sliding(2).foreach { pair =>
      g.drawLine(pair(0).x, pair(0).y, pair(1).x, pair(1).y)
    }
    g.setColor(Color.CYAN)
    intermediates.foreach { p =>
      drawCircle(p.x, p.y, 2)
    }
    g.setColor(Color.RED)
    val start = route.head
    drawCircle(start.x, start.y, 4)

    g.setColor(Color.GREEN)
    val end = route.last
    drawCircle(end.x, end.y, 4)

    ImageIO.write(bi, "PNG", new File(name))

  }
}

object a extends App {
  // val costMap = Array.ofDim[Int](7, 7)
  val costMap = Array.fill(7, 7)(1)
  // costMap(6)(6) = 11
  // costMap(0)(0) = 1
  // costMap(6)(6) = 1
  val startPoint = Point(0, 0)
  val endPoint = Point(6, 6)

  val b = new Astar(startPoint, endPoint, costMap)
  println("The Actual Path is " + b.execute().mkString(" "))
  costMap(5)(5) = 10
  println("Obstacle placed at x= 5 and y = 5")
  val c = new Astar(startPoint, endPoint, costMap)
  println("The path after obstacle is placed " + c.execute().mkString(" "))
  costMap(3)(3) = 10
  val d = new Astar(startPoint, endPoint, costMap)
  println("The path after obstacle is placed " + d.execute().mkString(" "))
}

object aLarge extends App {
  val costMap = Array.ofDim[Int](200, 200)
  val startPoint = Point(0, 0)
  val endPoint = Point(150, 150)

  val b = new Astar(startPoint, endPoint, costMap)
  println("The Actual Path is " + b.execute().mkString(" "))
  for (i <- 50 to 100; j <- 50 to 100) {
    costMap(i)(j) = 100
  }
  val c = new Astar(startPoint, endPoint, costMap)
  println("The path after obstacle is placed " + c.execute().mkString(" "))
}