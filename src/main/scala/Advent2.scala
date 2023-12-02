import java.nio.file.{Files, Path}

object Advent2:

  def main(args: Array[String]): Unit = {

    val lines = Files.readString(Path.of("./inputs/input2.txt")).linesIterator.toSeq

    val games = lines.map(parseGame)

  }

  def parseGame(line: String): Game = {
    // Game 1: 4 green, 7 blue; 2 blue, 4 red; 5 blue, 2 green, 2 red; 1 green, 3 red, 9 blue; 3 green, 9 blue; 7 green, 2 blue, 2 red
    val Seq(rawGame, rawDraws) = line.split(":", 2).toSeq
    val Seq(_, id) = rawGame.split(" ", 2).toSeq
    val draws = rawDraws.split(";").map { rawDraw =>
      val amounts = rawDraw.split(",").map(_.trim.toInt).toVector
      Draw(amounts)
    }.toSeq
    Game(id.toInt, draws)
  }

  case class Game(id: Int, draws: Seq[Draw])

  case class Draw(amounts: Vector[Int])