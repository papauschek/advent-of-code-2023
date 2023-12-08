import java.nio.file.{Files, Path}
import scala.annotation.tailrec

object Advent6:

  def main(args: Array[String]): Unit = {

    val rawLines = Files.readString(Path.of("./inputs/input6.txt")).linesIterator.toSeq
    val lines = rawLines.map(_.replace(" ", "")) // only for part 2
    val numbers = lines.map(_.split(':').last.split(' ').filter(_.nonEmpty).map(_.toLong).toSeq)
    val games = numbers(0).zip(numbers(1)).map((Game.apply _).tupled)

    def calculateOptions(game: Game): Long = {
      // solve quadratic equation for: (maxTime - buttonTime) * buttonTime = distance
      val sqrt = math.sqrt(game.maxTime * game.maxTime - 4 * game.distance)
      val result1 = (game.maxTime - sqrt) / 2
      val result2 = (game.maxTime + sqrt) / 2
      val low = (result1 + 0.00001).ceil.toLong
      val high = (result2 - 0.00001).floor.toLong
      val options = high - low + 1
      options
    }

    println(games.map(calculateOptions)) // 37286485
  }

  case class Game(maxTime: Long, distance: Long)

