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
    def countPathLength(start: Point, previous: Point = Point(-1, -1), count: Int = 0): Int = {
      val options = mazeConnections(start.y)(start.x)
      val nextPoint = options.find(_ != previous).head
      if (maze(nextPoint.y)(nextPoint.x) == 'S') {
        count + 1
      } else {
        countPathLength(nextPoint, start, count + 1)
      }
    }

    println(countPathLength(start) / 2)

  }

case class Point(x: Int, y: Int)


