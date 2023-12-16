import java.nio.file.{Files, Path}
import scala.annotation.tailrec

object Advent14:

  case class Point(x: Int, y: Int)

  def main(args: Array[String]): Unit = {

    val pattern = Files.readString(Path.of("./inputs/input14.txt")).linesIterator.toVector

    def rollPattern(pattern: Vector[String], direction: Point): Vector[String] = {
      var newPattern = pattern
      val isIndex1Vertical = direction.x == 0
      val isIndex1Reverse = direction.x > 0 || direction.y > 0
      var rawIndex1 = 0
      while (rawIndex1 < pattern.size) {
        val index1 = if (isIndex1Reverse) pattern.size - rawIndex1 - 1 else rawIndex1
        var index2 = 0
        while (index2 < pattern.size) {
          val y = if (isIndex1Vertical) index1 else index2
          val x = if (isIndex1Vertical) index2 else index1
          val char = newPattern(y)(x)
          if (char == 'O') {

            var newPoint = Point(x + direction.x, y + direction.y)
            while (newPoint.y >= 0 && newPoint.x >= 0
                && newPattern.length > newPoint.y
                && newPattern(newPoint.y).length > newPoint.x
                && newPattern(newPoint.y)(newPoint.x) == '.') {
              newPoint = Point(newPoint.x + direction.x, newPoint.y + direction.y)
            }
            newPoint = Point(newPoint.x - direction.x, newPoint.y - direction.y)

            newPattern = newPattern.updated(y, newPattern(y).updated(x, '.'))
            newPattern = newPattern.updated(newPoint.y, newPattern(newPoint.y).updated(newPoint.x, 'O'))
          }
          index2 += 1
        }
        rawIndex1 += 1
      }
      newPattern
    }

    def calculateLoad(pattern: Vector[String]): Int = {
      pattern.zipWithIndex.map {
        (y, index) => y.count(_ == 'O') * (pattern.length - index)
      }.sum
    }

    def rollCycle(pattern: Vector[String]): Vector[String] = {
      val rolledPattern = rollPattern(pattern, direction = Point(0, -1)) // north
      val rolledPattern2 = rollPattern(rolledPattern, direction = Point(-1, 0)) // west
      val rolledPattern3 = rollPattern(rolledPattern2, direction = Point(0, 1)) // south
      rollPattern(rolledPattern3, direction = Point(1, 0)) // east
    }

    val cycleFromPattern = collection.mutable.Map.empty[Vector[String], (Vector[String], Int)]
    var cacheRunCounter = 0

    def rollCycleCached(pattern: Vector[String]): (Vector[String], Int) = {
      cycleFromPattern.getOrElseUpdate(pattern, {
        cacheRunCounter += 1
        (rollCycle(pattern), cacheRunCounter)
      })
    }

    def determineRepetitionCount(pattern: Vector[String]): (Int, Int) = {
      var (rolledPattern, runCount) = rollCycleCached(pattern)
      var count = 1
      while (runCount == count) {
        val (newRolledPattern, newRunCount) = rollCycleCached(rolledPattern)
        rolledPattern = newRolledPattern
        runCount = newRunCount
        count += 1
      }
      (count, runCount)
    }

    @tailrec
    def rollCycleCount(pattern: Vector[String], count: Int): Vector[String] = {
      if (count <= 0) pattern
      else rollCycleCount(rollCycleCached(pattern)._1, count - 1)
    }

    val (endCount, startCount) = determineRepetitionCount(pattern)
    val remaining = (1000000000 - startCount) % (endCount - startCount)
    val rolledPattern = rollCycleCount(pattern, endCount + remaining)
    println(calculateLoad(rolledPattern)) // 102829
  }

