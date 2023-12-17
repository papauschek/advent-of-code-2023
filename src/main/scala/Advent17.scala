import java.nio.file.{Files, Path}
import scala.annotation.{tailrec, targetName}
import scala.collection.mutable

object Advent17:

  def main(args: Array[String]): Unit = {

    val rawGrid = Files.readString(Path.of("./inputs/input17.txt")).linesIterator.toVector
    val costsMap = rawGrid.map(_.map(_.toString.toInt).toVector)

    //println(costsMap.mkString("\r\n"))

    val allDirections = List(Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0))

    val allNodes: Map[Node, NodeValue] = (for
      y <- costsMap.indices
      x <- costsMap(y).indices
      direction <- allDirections
      lineLength <- 1 to 3
      node = Node(Point(x, y), direction, lineLength)
    yield (node, new NodeValue(node))).toMap


    val destinationPoint = Point(rawGrid.length - 1, rawGrid.length - 1)
    val destinationNodes = allNodes.keySet.filter(n => n.location == destinationPoint)

    val startNode = Node(Point(0, 0), Point(1, 0), lineLength = 1)
    val startNodeValue = allNodes(startNode)
    startNodeValue.distance = 0

    var unvisitedNodes = allNodes.values.toList

    var hasReachedDestination = false
    var count = 0
    while (!hasReachedDestination) {

      count += 1
      if (count % 1000 == 0) {
        println((count, allNodes.size))
      }

      val current = unvisitedNodes.minBy(_.distance)
      unvisitedNodes = unvisitedNodes.filterNot(_ == current)
      val currentNode = current.node
      hasReachedDestination = currentNode.location == destinationPoint

      require(current.distance != Int.MaxValue)
      allDirections.foreach {
        direction =>
          val lineLength = if (direction == currentNode.direction) currentNode.lineLength + 1 else 1
          val isForwardOrTurn = direction.x != -currentNode.direction.x || direction.y != -currentNode.direction.y
          if (lineLength <= 3 && isForwardOrTurn) {
            val nextNode = Node(currentNode.location + direction, direction, lineLength)
            allNodes.get(nextNode) match {
              case Some(nodeValue) =>
                val newDistance = current.distance + costsMap(nextNode.location.y)(nextNode.location.x)
                if (newDistance < nodeValue.distance) {
                  nodeValue.distance = newDistance
                  nodeValue.previous = Some(currentNode)
                }
              case _ =>
            }
          }
      }
    }

    var path = List.empty[Node]
    var node = destinationNodes.minBy(allNodes(_).distance)
    while (node.location != startNode.location) {
      path +:= node
      node = allNodes(node).previous.get
    }

    path.takeRight(3).foreach  {
      node => println((node, allNodes(node)))
    }

    val pathPoints = path.map(_.location).toSet



    //println(pathPoints.mkString("\r\n"))

    /*
    for {
      y <- costsMap.indices
    } yield {
      println(costsMap(y).indices.map(x => {
        val point = Point(x, y)
        if (pathPoints.contains(point)) {
          "X"
        } else {
          costsMap(y)(x).toString
        }
      }).mkString)
    }
     */


  }

  case class Point(x: Int, y: Int) {
    def +(other: Point): Point = Point(x + other.x, y + other.y)
  }

  case class Node(location: Point, direction: Point, lineLength: Int)

  class NodeValue(val node: Node,
                  var distance: Int = Int.MaxValue,
                  var previous: Option[Node] = None) {

    override def toString: String = s"NodeValue(distance=$distance, previous=${previous.map(_.location)})"

  }