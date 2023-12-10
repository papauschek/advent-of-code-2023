import java.nio.file.{Files, Path}
import scala.annotation.tailrec

object Advent10:

  def main(args: Array[String]): Unit = {

    val maze = Files.readString(Path.of("./inputs/input10.txt")).linesIterator.toVector

    val pipes = Map(
      '|' -> Seq(Point(0, -1), Point(0, 1)),
      '-' -> Seq(Point(1, 0), Point(-1, 0)),
      'F' -> Seq(Point(1, 0), Point(0, 1)),
      '7' -> Seq(Point(0, 1), Point(-1, 0)),
      'J' -> Seq(Point(0, -1), Point(-1, 0)),
      'L' -> Seq(Point(0, -1), Point(1, 0)),
      'S' -> Seq(Point(0, -1), Point(1, 0), Point(0, 1), Point(-1, 0)),
      '.' -> Nil
    )

    val startY = maze.indexWhere(_.contains('S'))
    val start = Point(maze(startY).indexOf('S'), startY)

    val mazeConnections: Vector[Vector[Seq[Point]]] = for {
      (line, y) <- maze.zipWithIndex
    } yield {
      for {
        (char, x) <- maze(y).zipWithIndex.toVector
      } yield {
        pipes(char).flatMap {
          p =>
            val neighbor = Point(x + p.x, y + p.y)
            if (neighbor.x >= 0 && neighbor.y >= 0 && neighbor.y < maze.length && neighbor.x < maze(neighbor.y).length) {
              val neighborChar = maze(neighbor.y)(neighbor.x)
              val neighborPipes = pipes(neighborChar)
              val isConnected = neighborPipes.exists(n => n.x == -p.x && n.y == -p.y)
              Option.when(isConnected)(neighbor)
            } else {
              None
            }
        }
      }
    }

    @tailrec
    def getPath(start: Point, previous: Point = Point(-1, -1), path: List[Point] = Nil): List[Point] = {
      val options = mazeConnections(start.y)(start.x)
      val nextPoint = options.find(_ != previous).head
      if (maze(nextPoint.y)(nextPoint.x) == 'S') {
        nextPoint :: path
      } else {
        getPath(nextPoint, start, nextPoint :: path)
      }
    }

    val path = getPath(start).toSet

    @tailrec
    def getIntersectionCount(point: Point, intersectionCount: Int = 0, contiguous: Int = 0): Int = {
      val isIntersection = path.contains(point)
      val hasMore = point.x < maze(point.y).length - 1

      val (totalIntersectionCount, totalContiguous) =
        if (isIntersection) {
          if (hasMore) {
            (intersectionCount, contiguous + 1)
          } else {
            val newIntersections = countIntersections(point.copy(x = point.x + 1), contiguous + 1)
            (intersectionCount + newIntersections, 0)
          }
        } else {
          if (contiguous > 0) {
            val newIntersections = countIntersections(point, contiguous)
            (intersectionCount + newIntersections, 0)
          } else {
            (intersectionCount, 0)
          }
        }

      if (hasMore) {
        getIntersectionCount(Point(point.x + 1, point.y), totalIntersectionCount, totalContiguous)
      } else {
        totalIntersectionCount
      }
    }

    def countIntersections(endPoint: Point, length: Int): Int = {
      val section = maze(endPoint.y).substring(endPoint.x - length, endPoint.x)
      var sectionX = 0
      var intersectionCount = 0
      var lastDirection = Option.empty[Int]
      while (sectionX < section.length) {
        val char = section(sectionX)
        if (char == '|') {
          intersectionCount += 1
        } else if (char != '.' && char != '-') {
          val connections = mazeConnections(endPoint.y)(sectionX + endPoint.x - length)
          val nextDirection = connections.map(c => c.y - endPoint.y).find(_ != 0).get
          if (lastDirection.contains(nextDirection)) {
            lastDirection = None // pipe going back in the same direction, no intersection
          } else if (lastDirection.isDefined) {
            lastDirection = None // pipe crossing, add intersection
            intersectionCount += 1
          } else {
            lastDirection = Some(nextDirection)
          }
        }
        sectionX += 1
      }
      intersectionCount
    }

    val enclosedPoints = for {
      (line, y) <- maze.zipWithIndex
      (char, x) <- maze(y).zipWithIndex.toVector
      point = Point(x, y)
      if !path.contains(point) && getIntersectionCount(point) % 2 == 1
    } yield point

    println(mazeConnections(start.y)(start.x))
    println()
    println(enclosedPoints.mkString("\r\n"))
    println(enclosedPoints.length)

  }

case class Point(x: Int, y: Int)


