import java.nio.file.{Files, Path}

object Advent13:

  def main(args: Array[String]): Unit = {

    val rawPatterns = Files.readString(Path.of("./inputs/input13.txt")).split("\n\n").toSeq
    val patterns = rawPatterns.map(_.linesIterator.toVector)

    def isHorizontalReflection(y: Int, pattern: Vector[String]): Boolean = {
      (0 until (pattern.length - y).min(y)).forall {
        dy =>
          (0 until pattern(y).length).forall {
            x => pattern(y + dy)(x) == pattern(y - 1 - dy)(x)
          }
      }
    }

    def isVerticalReflection(x: Int, pattern: Vector[String]): Boolean = {
      (0 until (pattern(0).length - x).min(x)).forall {
        dx =>
          pattern.forall {
            y => y(x + dx) == y(x - 1 - dx)
          }
      }
    }

    def getReflections(pattern: Vector[String]): Reflections = {
      val horizontalReflections = (1 until pattern.length).filter(y => isHorizontalReflection(y, pattern))
      val verticalReflections = (1 until pattern(0).length).filter(x => isVerticalReflection(x, pattern))
      Reflections(horizontalReflections, verticalReflections)
    }

    def getAlternativeReflections(pattern: Vector[String]): Reflections = {
      val originalReflection = getReflections(pattern)
      (for {
        y <- pattern.indices
        x <- pattern(y).indices
        newChar = if (pattern(y)(x) == '.') '#' else '.'
        newPattern = pattern.updated(y, pattern(y).updated(x, newChar))
        reflections = getReflections(newPattern)
        newHorizontal = reflections.horizontal.filterNot(originalReflection.horizontal.contains)
        newVertical = reflections.vertical.filterNot(originalReflection.vertical.contains)
        if newHorizontal.nonEmpty || newVertical.nonEmpty
      } yield Reflections(newHorizontal, newVertical)).head
    }

    val reflections = patterns.map(getAlternativeReflections)
    println(reflections.flatMap(_.vertical).sum + reflections.flatMap(_.horizontal).sum * 100) // 22906
  }

  case class Reflections(horizontal: Seq[Int], vertical: Seq[Int])

