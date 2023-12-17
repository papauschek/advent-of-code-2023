import java.nio.file.{Files, Path}
import scala.annotation.tailrec

object Advent15:

  def main(args: Array[String]): Unit = {

    val line = Files.readString(Path.of("./inputs/input15.txt")).linesIterator.toList.head
    val steps = line.split(",").toSeq.map {
      s =>
        val label = s.takeWhile(c => c != '-' && c != '=')
        val isEquals = s(label.length) == '='
        val focalLength = if (isEquals) s.drop(label.length + 1).toInt else 0
        Step(label, isEquals, focalLength)
    }

    def calculateHash(input: String): Int = {
      var currentValue = 0
      var index = 0
      while (index < input.length) {
        currentValue = ((currentValue + input.charAt(index)) * 17) % 256
        index += 1
      }
      currentValue
    }

    val boxes = (0 until 256).map(index => new Box(index)).toVector

    steps.foreach {
      step =>

        val hash = calculateHash(step.label)
        val box = boxes(hash)

        if (step.isEquals) {
          box.lenses.indexWhere(_.label == step.label) match {
            case -1 => box.lenses = box.lenses :+ new Lens(step.label, step.focalLength)
            case index => box.lenses(index).focalLength = step.focalLength
          }
        } else {
          box.lenses = box.lenses.filter(_.label != step.label)
        }
    }

    println(boxes.map(_.focusingPower).sum) // 284674

  }

  case class Step(label: String, isEquals: Boolean, focalLength: Int)

  class Box(val index: Int, var lenses: Vector[Lens] = Vector.empty) {

    override def toString: String = s"Box $index: ${lenses.mkString(", ")}"

    def focusingPower: Int = lenses.zipWithIndex.map {
      case (lens, lensIndex) => lens.focalLength * (lensIndex + 1) * (index + 1)
    }.sum

  }

  class Lens(val label: String, var focalLength: Int) {
    override def toString: String = s"$label=$focalLength"
  }