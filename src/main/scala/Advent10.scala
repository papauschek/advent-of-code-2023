import java.nio.file.{Files, Path}
import scala.annotation.tailrec

object Advent10:

  case class Point(x: Int, y: Int)

  def main(args: Array[String]): Unit = {

    val maze = Files.readString(Path.of("./inputs/input10.txt")).linesIterator.toVector

    // map of pipe characters to the directions they connect to
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

    // get starting point
    val startY = maze.indexWhere(_.contains('S'))
    val start = Point(maze(startY).indexOf('S'), startY)

    // get all the valid neighboring connections for each location in the maze
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

    // get the pipe path from the start to the end
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

    // count the number of intersections from given point to the right
    @tailrec
    def getIntersectionCount(point: Point, intersectionCount: Int = 0, contiguous: Int = 0): Int = {
      val isIntersection = path.contains(point)
      val hasMore = point.x < maze(point.y).length - 1

      val (totalIntersectionCount, totalContiguous) =
        if (isIntersection) {
          if (hasMore) {
            (intersectionCount, contiguous + 1)
          } else {
            val newIntersections = countPathIntersections(point.copy(x = point.x + 1), contiguous + 1)
            (intersectionCount + newIntersections, 0)
          }
        } else {
          if (contiguous > 0) {
            val newIntersections = countPathIntersections(point, contiguous)
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

    // count intersections in a horizontal section of the path
    // some sections along the horizontal section are not intersections if they can be avoided (e.g. "F-7" or "L-J", but not "F-J" or "L-7")
    def countPathIntersections(endPoint: Point, length: Int): Int = {
      val section = maze(endPoint.y).substring(endPoint.x - length, endPoint.x)
      var sectionX = 0
      var intersectionCount = 0
      var lastDirection = Option.empty[Int]
      while (sectionX < section.length) {
        val char = section(sectionX)
        val connections = mazeConnections(endPoint.y)(sectionX + endPoint.x - length)
        val verticalConnections = connections.map(c => c.y - endPoint.y).filter(_ != 0)
        if (verticalConnections.length == 2) {
          intersectionCount += 1
        } else if (verticalConnections.nonEmpty) {
          val nextDirection = verticalConnections.head
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

    // if a point is enclosed by a polygon, the number of intersections from that point to the right will be odd
    val enclosedPoints = (for {
      (line, y) <- maze.zipWithIndex
      (char, x) <- maze(y).zipWithIndex.toVector
      point = Point(x, y)
      if !path.contains(point) && getIntersectionCount(point) % 2 == 1
    } yield point).toSet

    // output solutions
    println(toDisplayString(maze, path, enclosedPoints))
    println()
    println(path.size / 2) // Part 1: 6903
    println(enclosedPoints.size) // Part 2: 265
  }

  // display maze with enclosed points ("I"), with pipes outside the path removed.
  private def toDisplayString(maze: Vector[String], path: Set[Point], enclosedPoints: Set[Point]): String = {
    (for {
      (line, y) <- maze.zipWithIndex
    } yield {
      (for {
        (char, x) <- maze(y).zipWithIndex.toVector
      } yield {
        if (path.contains(Point(x, y))) {
          char
        } else if (enclosedPoints.contains(Point(x, y))) {
          'I'
        } else {
          ' '
        }
      }).mkString
    }).mkString("\r\n")
  }




