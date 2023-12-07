import java.nio.file.{Files, Path}
import scala.annotation.tailrec

object Advent5:

  def main(args: Array[String]): Unit = {

    val lines = Files.readString(Path.of("./inputs/input5.txt")).linesIterator.toSeq

    val seeds = lines.head.split(':').last.trim.split(' ').map(_.toLong).toSeq
    val seedRanges = seeds.grouped(2).toSeq.map(group => Range(group.head, group.last))

    val rawMaps = lines.drop(2).mkString("\n").split("\n\n").toSeq

    def parseMapping(rawMap: String): Mapping = {
      val mapLines = rawMap.linesIterator.toSeq
      val name = mapLines.head.split(' ').head // e.g. "seed-to-soil"
      val ranges = mapLines.drop(1).map(parseDestination)
      Mapping(name, ranges)
    }

    def parseDestination(rawRange: String): Destination = {
      val parts = rawRange.split(' ')
      val destinationStart = parts(0).toLong
      val sourceStart = parts(1).toLong
      val sourceCount = parts(2).toLong
      Destination(destinationStart, Range(sourceStart, sourceCount))
    }

    val maps: List[Mapping] = rawMaps.map(parseMapping).toList

    def resolveLocation(inputRange: Range, remainingMaps: List[Mapping] = maps): Seq[Range] = {
      if (remainingMaps.isEmpty) {
        Seq(inputRange)
      } else {
        val map = remainingMaps.head

        var remainingRanges = Seq(inputRange)
        var outputRanges = Seq.empty[Range]
        map.destinations.foreach {
          destination =>
            remainingRanges = remainingRanges.flatMap {
              remainingRange =>
                val (intersection, remainder) = remainingRange.intersection(destination.source)
                val offset = intersection.offset(destination.destinationOffset)
                outputRanges = outputRanges :+ offset
                remainder
            }
        }

        val outputs = outputRanges.filter(_.count > 0) ++ remainingRanges
        outputs.flatMap(range => resolveLocation(range, remainingMaps.tail))
      }
    }

    println(seedRanges.flatMap(resolveLocation(_)).map(_.start).min) // 136096660

  }

  case class Mapping(name: String, destinations: Seq[Destination])

  case class Destination(destinationStart: Long,
                         source: Range) {

    val destinationOffset: Long = destinationStart - source.start

  }

  case class Range(start: Long, count: Long) {

    // non-inclusive
    val end: Long = start + count

    /** return intersection range and remainder ranges (if any), not contained in the other range */
    def intersection(other: Range): (Range, Seq[Range]) = {
      if (other.start >= end || other.end <= start) {
        (Range(0, 0), Seq(this)) // no intersection
      } else {
        val intersectionStart = Math.max(start, other.start)
        val intersectionEnd = Math.min(end, other.end)
        val intersection = Range(intersectionStart, intersectionEnd - intersectionStart)
        val remainder = Seq(
          Range(start, intersectionStart - start),
          Range(intersectionEnd, end - intersectionEnd)
        ).filter(_.count > 0)
        (intersection, remainder)
      }
    }

    def offset(offset: Long): Range = copy(start = start + offset)

  }

