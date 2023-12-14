import java.nio.file.{Files, Path}

object Advent14:

  def main(args: Array[String]): Unit = {

    val pattern = Files.readString(Path.of("./inputs/input14.txt")).linesIterator.toVector

    def rollPattern(pattern: Vector[String]): Vector[String] = {
      var newPattern = pattern
      var y = 0
      while (y < pattern.size) {
        var x = 0
        while (x < pattern.size) {
          val char = newPattern(y)(x)
          if (char == 'O') {
            val newY = (0 until y).findLast(y2 => newPattern(y2)(x) != '.') match {
              case Some(last) => last + 1
              case _ => 0
            }
            newPattern = newPattern.updated(y, newPattern(y).updated(x, '.'))
            newPattern = newPattern.updated(newY, newPattern(newY).updated(x, 'O'))
          }
          x += 1
        }
        y += 1
      }
      newPattern
    }

    def calculateLoad(pattern: Vector[String]): Int = {
      pattern.zipWithIndex.map {
        (y, index) => y.count(_ == 'O') * (pattern.length - index)
      }.sum
    }

    val rolledPattern = rollPattern(pattern)
    println(calculateLoad(rolledPattern))
    //println(rollPattern(patterns(0)).mkString("\r\n"))

  }

