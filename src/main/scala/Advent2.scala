import java.nio.file.{Files, Path}

object Advent2:

  def main(args: Array[String]): Unit = {
    val lines = Files.readString(Path.of("./inputs/input2.txt")).linesIterator.toSeq
    val games = lines.map(parseGame)
    println(games.map(_.power).sum) // 77021
  }

  val colors: Vector[String] = Vector("red", "green", "blue")

  case class Game(id: Int, draws: Seq[Vector[Int]], max: Vector[Int]) {

    def power: Long = max(0) * max(1) * max(2)

  }

  // e.g. Game 1: 4 green, 7 blue; 2 blue, 4 red; 5 blue, 2 green, 2 red
  def parseGame(line: String): Game = {
    val Seq(rawGame, rawDraws) = line.split(":", 2).toSeq
    val Seq(_, id) = rawGame.split(" ", 2).toSeq
    val draws = rawDraws.split(";").map(parseDraw).toSeq
    val max = colors.indices.map(i => draws.map(_(i)).max).toVector
    Game(id.toInt, draws, max)
  }

  // e.g. "4 green, 7 blue"
  def parseDraw(rawDraw: String): Vector[Int] = {
    val amounts = rawDraw.split(",").map(_.trim).toVector.map {
      rawAmount =>
        val Seq(rawNumber, rawColor) = rawAmount.split(" ", 2).toSeq
        (rawColor, rawNumber.toInt)
    }.toMap

    colors.map(color => amounts.getOrElse(color, 0))
  }

