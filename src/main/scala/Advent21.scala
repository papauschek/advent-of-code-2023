import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.collection.mutable

object Advent21:

  def main(args: Array[String]): Unit = {

    val lines = Files.readString(Path.of("./inputs/input21.txt")).linesIterator.toVector

    val startingPoint = {
      val y = lines.indexWhere(_.contains('S'))
      val x = lines(y).indexOf('S')
      Point(x, y)
    }

    val distances = Array.fill(lines.size, lines.head.size)(Int.MaxValue)
    distances(startingPoint.y)(startingPoint.x) = 0
    val targetDistance = 64 //  64
    var currentDistance = 0
    val remainingPoints = mutable.Set(startingPoint)
    val visitedPoints = mutable.Set.empty[Point]

    while (remainingPoints.nonEmpty) {
      val currentPoint = remainingPoints.minBy(p => distances(p.y)(p.x))
      remainingPoints -= currentPoint
      val wasAdded = visitedPoints.add(currentPoint)
      val currentDistance = distances(currentPoint.y)(currentPoint.x)
      if (wasAdded && currentDistance < targetDistance) {
        currentPoint.neighbors.foreach {
          n =>
            if (n.y >= 0 && n.y < lines.length
              && n.x >= 0 && n.x < lines(n.y).length
              && lines(n.y)(n.x) != '#') {
              val newDistance = currentDistance + 1
              if (newDistance < distances(n.y)(n.x)) {
                distances(n.y)(n.x) = newDistance
                remainingPoints += n
              }
            }
        }
      }
    }
    
    for {
      y <- lines.indices
      x <- lines(y).indices
    } {
      if (lines(y)(x) == '#' || lines(y)(x) == 'S') print(lines(y)(x))
      else if (distances(y)(x) == Int.MaxValue) print(".")
      else print(distances(y)(x) % 10)
      if (x == lines(y).length - 1) println()
    }

    println(distances.flatten.count(d => d <= targetDistance && d % 2 == 0)) // 3632

  }

  case class Point(x: Int, y: Int) {
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def neighbors: List[Point] = List(Point(x + 1, y), Point(x - 1, y), Point(x, y + 1), Point(x, y - 1))
  }