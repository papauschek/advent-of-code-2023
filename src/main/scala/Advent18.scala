import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.collection.mutable

object Advent18:

  def main(args: Array[String]): Unit = {

    val rawLines = Files.readString(Path.of("./inputs/input18.txt")).linesIterator.toSeq
    val steps = rawLines.map {
      line => line.split(' ').toSeq match {
        case Seq(dir, length, color) =>
          val direction = dir match {
            case "U" => Point(0, -1)
            case "D" => Point(0, 1)
            case "L" => Point(-1, 0)
            case "R" => Point(1, 0)
          }
          Step(direction, length.toInt, color.substring(2, 8))
      }
    }

    //println(steps.mkString("\r\n"))

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

    //println(lines.mkString("\r\n"))

    def calculatePolygonArea(polygon: List[Point]): Int = {

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
      println((area, borderArea))
      area + borderArea
    }

    println(calculatePolygonArea(points)) // 39039
  }

  case class Step(direction: Point, length: Int, color: String)

  case class Point(x: Int, y: Int) {
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def *(factor: Int): Point = Point(x * factor, y * factor)
  }
