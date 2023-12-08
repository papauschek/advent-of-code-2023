import java.nio.file.{Files, Path}
import scala.annotation.tailrec

object Advent8:

  def main(args: Array[String]): Unit = {

    val rawLines = Files.readString(Path.of("./inputs/input8.txt")).linesIterator.toSeq
    val instructions = rawLines.head.toCharArray
    val removedChars = Set('=', '(', ')', ',')
    val rawElements = rawLines.drop(2).map(_.filterNot(removedChars).split(" ").toSeq.filter(_.nonEmpty))

    val elements = rawElements.map {
      case Seq(key, left, right) => Element(key, left, right)
    }.map(e => e.key -> e).toMap

    val startElements = elements.keys.toSeq.filter(_.endsWith("A"))

    val steps = startElements.map {
      startElement => calculateSteps(instructions, elements, startElement)
    }

    // the time when all the steps "meet" will be a multiple of all of them
    // so we can just fold over them to find the lowest common multiple
    // it turns out that all the steps are divisible by [instructions.length], so we can factor this out
    println(steps.map(_ / instructions.length).fold(1L)(_ * _) * instructions.length) // 9064949303801
  }

  /** calculate number of steps for each start element to reach the end */
  private def calculateSteps(instructions: Array[Char], elements: Map[String, Element], start: String): Long = {
    var current = start
    var count = 0L
    var instructionIndex = 0
    var done = false
    while (!current.endsWith("Z")) {
      count += 1
      val instruction = instructions(instructionIndex)
      val nextElement = instruction match {
        case 'L' => elements(current).left
        case 'R' => elements(current).right
      }
      current = nextElement
      instructionIndex = (instructionIndex + 1) % instructions.length
    }
    count
  }

  case class Solution(instructionIndex: Int, element: String)

  case class Element(key: String, left: String, right: String)

