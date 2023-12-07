import java.nio.file.{Files, Path}
import scala.annotation.tailrec

object Advent5:

  def main(args: Array[String]): Unit = {

    val lines = Files.readString(Path.of("./inputs/input5.txt")).linesIterator.toSeq

    val seeds = lines.head.split(':').last.trim.split(' ').map(_.toLong).toSeq

    val rawMaps = lines.drop(2).mkString("\n").split("\n\n").toSeq

    def parseMapping(rawMap: String): Mapping = {
      val mapLines = rawMap.linesIterator.toSeq
      val name = mapLines.head.split(' ').head // e.g. "seed-to-soil"
      val ranges = mapLines.drop(1).map(parseRange)
      Mapping(name, ranges)
    }

    def parseRange(rawRange: String): Range = {
      val parts = rawRange.split(' ')
      val destinationStart = parts(0).toLong
      val sourceStart = parts(1).toLong
      val sourceCount = parts(2).toLong
      Range(destinationStart, sourceStart, sourceCount)
    }

    val maps: List[Mapping] = rawMaps.map(parseMapping).toList

    @tailrec
    def resolveLocation(seed: Long, remainingMaps: List[Mapping] = maps): Long = {
      if (remainingMaps.isEmpty) {
        seed
      } else {
        val map = remainingMaps.head
        val nextNumber = map.ranges.find(range => range.sourceStart <= seed && range.sourceEnd > seed) match {
          case Some(range) => seed + range.destinationOffset
          case _ => seed
        }
        resolveLocation(nextNumber, remainingMaps.tail)
      }
    }

    println(seeds.map(resolveLocation(_)).min)

  }

  case class Mapping(name: String, ranges: Seq[Range])

  case class Range(destinationStart: Long,
                   sourceStart: Long,
                   sourceCount: Long) {

    val sourceEnd: Long = sourceStart + sourceCount
    val destinationOffset: Long = destinationStart - sourceStart

  }

