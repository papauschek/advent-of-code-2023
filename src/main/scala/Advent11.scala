import java.nio.file.{Files, Path}
import scala.annotation.tailrec

object Advent11:

  case class Point(x: Long, y: Long)

  def main(args: Array[String]): Unit = {

    val image = Files.readString(Path.of("./inputs/input11.txt")).linesIterator.toVector

    val multiplier = 1000000L // Part 1: 1L, Part 2: 1000000L

    val galaxies: Vector[Point] = for {
      (line, y) <- image.zipWithIndex
      (char, x) <- image(y).zipWithIndex.toVector if char == '#'
    } yield Point(x, y)

    // get all rows and columns with no galaxies
    val emptyRows = image.zipWithIndex.filter(_._1.forall(_ == '.')).map(_._2)
    val emptyCols = image.transpose.zipWithIndex.filter(_._1.forall(_ == '.')).map(_._2)

    val expandedGalaxies = galaxies.map {
      galaxy =>
        val expandedRows = emptyRows.count(_ < galaxy.y).toLong
        val expandedCols = emptyCols.count(_ < galaxy.x).toLong
        Point(galaxy.x + expandedCols * (multiplier - 1), galaxy.y + expandedRows * (multiplier - 1))
    }

    val distances = for {
      (galaxy1, index) <- expandedGalaxies.zipWithIndex
      galaxy2 <- expandedGalaxies.drop(index + 1)
    } yield {
      (galaxy2.y - galaxy1.y).abs + (galaxy2.x - galaxy1.x).abs
    }

    println(distances.sum) // Part 1: 10276166, Part 2 : 598693078798

  }


