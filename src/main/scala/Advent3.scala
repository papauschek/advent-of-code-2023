import java.nio.file.{Files, Path}

object Advent3:

  def main(args: Array[String]): Unit = {

    val schematic = Files.readString(Path.of("./inputs/input3.txt")).linesIterator.toVector

    val symbols = for {
      (line, y) <- schematic.zipWithIndex
      (char, x) <- line.zipWithIndex if char == '*'
    } yield Point(x, y)

    def findDigits(point: Point, symbol: Point): NumberLocation = {
      val line = schematic(point.y)
      val lastIndex = line.indexWhere(!_.isDigit, point.x) match {
        case -1 => line.length
        case x => x
      }
      val number = line.take(lastIndex).reverse.takeWhile(_.isDigit).reverse
      val location = Point(lastIndex - number.length, point.y)
      NumberLocation(location, number.toLong, symbol)
    }

    val allNumbers = (for {
      symbol <- symbols
      neighbor <- symbol.neighbors if schematic(neighbor.y)(neighbor.x).isDigit
    } yield findDigits(neighbor, symbol)).distinct

    val allRatios: Seq[Long] = allNumbers.groupBy(_.symbol).map {
      case (symbol, numbers) =>
        if (numbers.length == 2) {
          numbers(0).number * numbers(1).number
        } else {
          0
        }
    }.toSeq

    println(allRatios.sum) // 84159075

  }

  case class NumberLocation(numberLocation: Point, number: Long, symbol: Point)

  case class Point(x: Int, y: Int) {

    def neighbors: Seq[Point] = Seq(
      Point(x + 1, y),
      Point(x - 1, y),
      Point(x, y + 1),
      Point(x, y - 1),
      Point(x + 1, y + 1),
      Point(x + 1, y - 1),
      Point(x - 1, y + 1),
      Point(x - 1, y - 1)
    )

  }
