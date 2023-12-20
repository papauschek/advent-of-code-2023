import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.collection.mutable

object Advent18:

  def main(args: Array[String]): Unit = {

    val rawLines = Files.readString(Path.of("./inputs/input18.txt")).linesIterator.toSeq
    val steps = rawLines.map {
      line => line.split(' ').toSeq match {
        case Seq(_, _, color) =>
          val len = Integer.parseInt(color.substring(2, 7), 16)
          val direction = color.substring(7, 8) match {
            case "0" => Point(1, 0)
            case "1" => Point(0, 1)
            case "2" => Point(-1, 0)
            case "3" => Point(0, -1)
          }
          Step(direction, len)
      }
    }

    var points = List.empty[Point]
    var currentPoint = Point(0, 0)
    steps.foreach {
      step =>
        currentPoint = currentPoint + step.direction * step.length
        points = currentPoint :: points
    }
    points = {
      val minX = points.map(_.x).min
      val minY = points.map(_.y).min
      points.map(p => Point(p.x - minX, p.y - minY)).reverse
    }

    def calculatePolygonArea(polygon: List[Point]): Long = {

      val doubleArea = (polygon ++ polygon.take(1)).sliding(2).map {
        case Seq(point1, point2) => point1.x * point2.y - point2.x * point1.y
      }.sum

      val borderArea = (polygon ++ polygon.take(2)).sliding(3).map {
        case Seq(point1, point2, point3) =>
          if (point2.y > point1.y) {
            if (point3.x < point2.x) {
              point2.y - point1.y + 1 // bottom left corner
            } else {
              point2.y - point1.y
            }
          } else if (point2.x < point1.x) {
            if (point3.y > point2.y) {
              point1.x - point2.x - 1 // compensate top left corner double counting
            } else {
              point1.x - point2.x
            }
          } else {
            0
          }
      }.sum

      val area = doubleArea / 2
      area + borderArea
    }

    println(calculatePolygonArea(points)) // 44644464596918
  }

  case class Step(direction: Point, length: Long)

  case class Point(x: Long, y: Long) {
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def *(factor: Long): Point = Point(x * factor, y * factor)
  }
