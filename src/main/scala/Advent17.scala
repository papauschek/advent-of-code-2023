import java.nio.file.{Files, Path}
import scala.collection.mutable

object Advent17:

  def main(args: Array[String]): Unit = {

    val rawGrid = Files.readString(Path.of("./inputs/input17.txt")).linesIterator.toVector
    val costsMap = rawGrid.map(_.map(_.toString.toInt).toVector)

    val allDirections = List(Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0))

    val startNode = Node(Point(0, 0), Point(0, 0), lineLength = 4)
    val allNodes: Map[Node, NodeValue] = ((for
      y <- costsMap.indices
      x <- costsMap(y).indices if x > 0 || y > 0
      direction <- allDirections
      lineLength <- 1 to 10
      node = Node(Point(x, y), direction, lineLength)
    yield (node, new NodeValue(node)))
      :+ (startNode, new NodeValue(startNode))).toMap

    val destinationPoint = Point(rawGrid(0).length - 1, rawGrid.length - 1)

    val startNodeValue = allNodes(startNode)
    startNodeValue.distance = 0

    // Dijkstra implementation
    val unvisitedNodes = collection.mutable.PriorityQueue(startNodeValue)(Ordering.by(-_.distance))
    val visitedNodes = collection.mutable.Set.empty[Node]
    var hasReachedDestination = false
    while (!hasReachedDestination) {

      val current = unvisitedNodes.dequeue()
      val currentNode = current.node
      hasReachedDestination = currentNode.location == destinationPoint

      if (visitedNodes.add(currentNode)) {
        allDirections.foreach {
          direction =>
            val isSameDirection = direction == currentNode.direction
            val lineLength = if (isSameDirection) currentNode.lineLength + 1 else 1
            val isForwardOrTurn = direction.x != -currentNode.direction.x || direction.y != -currentNode.direction.y
            val canStopOrTurn = currentNode.lineLength >= 4
            if ((canStopOrTurn || isSameDirection) && lineLength <= 10 && isForwardOrTurn) {
              val nextNode = Node(currentNode.location + direction, direction, lineLength)
              val isDestination = nextNode.location == destinationPoint
              allNodes.get(nextNode) match {
                case Some(nodeValue) if !isDestination || lineLength >= 4 =>
                  val newDistance = current.distance + costsMap(nextNode.location.y)(nextNode.location.x)
                  if (newDistance < nodeValue.distance) {
                    nodeValue.distance = newDistance
                    nodeValue.previous = Some(current)
                    unvisitedNodes.enqueue(nodeValue)
                  }
                case _ =>
              }
            }
        }
      }
    }

    // traverse and collect path from destination to start
    var path = List.empty[NodeValue]
    val destinationNodes = allNodes.values.filter(n => n.node.location == destinationPoint).toList
    var nodeValue = destinationNodes.minBy(_.distance)
    while (nodeValue.node.location != startNode.location) {
      path +:= nodeValue
      nodeValue = nodeValue.previous.get
    }

    displayPath(path)

    println(path.last.distance) // 1197
  }

  private def displayPath(path: List[NodeValue]): Unit = {
    val pathPoints = path.map(_.node.location).toSet
    val (maxX, maxY) = (path.map(_.node.location.x).max, path.map(_.node.location.y).max)
    for {
      y <- 0 to maxY
    } yield {
      println((0 to maxX).map(x => if (pathPoints.contains(Point(x, y))) "X" else " ").mkString)
    }
  }

  case class Point(x: Int, y: Int) {
    def +(other: Point): Point = Point(x + other.x, y + other.y)
  }

  case class Node(location: Point, direction: Point, lineLength: Int)

  class NodeValue(val node: Node,
                  var distance: Int = Int.MaxValue,
                  var previous: Option[NodeValue] = None) {

    override def toString: String = s"NodeValue(distance=$distance, previous=${previous.map(_.node.location)})"

  }