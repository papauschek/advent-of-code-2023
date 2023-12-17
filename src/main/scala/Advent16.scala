import java.nio.file.{Files, Path}
import scala.annotation.{tailrec, targetName}

object Advent16:

  def main(args: Array[String]): Unit = {

    val rawGrid = Files.readString(Path.of("./inputs/input16.txt")).linesIterator.toVector

    def simulateBeam(beam: Beam): Int = {
      val grid = rawGrid.map(line => line.map(char => new Field(char, Set.empty)).toVector)

      var remainingBeams: List[Beam] = List(beam)

      while (remainingBeams.nonEmpty) {

        val beam = remainingBeams.head
        remainingBeams = remainingBeams.tail

        val (point, direction) = (beam.point, beam.direction)

        if (point.y >= 0 && point.x >= 0 && point.y < grid.size && point.x < grid.head.size) {

          val field = grid(point.y)(point.x)

          val newDirections: Seq[Point] = if (field.char == '.') {
            Seq(direction)
          } else if (field.char == '\\') {
            Seq(Point(direction.y, direction.x))
          } else if (field.char == '/') {
            Seq(Point(-direction.y, -direction.x))
          } else if (field.char == '|') {
            if (direction.x == 0) {
              Seq(direction)
            } else {
              Seq(
                Point(direction.y, direction.x),
                Point(-direction.y, -direction.x)
              )
            }
          } else if (field.char == '-') {
            if (direction.y == 0) {
              Seq(direction)
            } else {
              Seq(
                Point(direction.y, direction.x),
                Point(-direction.y, -direction.x)
              )
            }
          } else {
            ???
          }

          newDirections.foreach {
            newDirection =>
              if (!field.directions.contains(newDirection)) {
                remainingBeams = Beam(point + newDirection, newDirection) :: remainingBeams
                field.directions += newDirection
              }
          }
        }
      }

      grid.map(_.count(_.directions.nonEmpty)).sum
    }

    val beams = rawGrid.indices.flatMap {
      index =>
        Seq(
          Beam(Point(0, index), Point(1, 0)),
          Beam(Point(index, 0), Point(0, 1)),
          Beam(Point(rawGrid.size - 1, index), Point(-1, 0)),
          Beam(Point(index, rawGrid.size - 1), Point(0, -1))
        )
    }

    val grids = beams.map(simulateBeam)

    println(grids.max) // 7943

  }

  case class Beam(point: Point, direction: Point)

  class Field(val char: Char, var directions: Set[Point]) {

    override def toString: String = char match {
      case '.' => if (directions.isEmpty) "." else directions.size.toString
      case _ => char.toString
    }

  }

  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)
