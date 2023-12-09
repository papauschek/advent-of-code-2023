import java.nio.file.{Files, Path}
import scala.annotation.tailrec

object Advent9:

  def main(args: Array[String]): Unit = {
    val rawLines = Files.readString(Path.of("./inputs/input9.txt")).linesIterator.toSeq
    val sequences = rawLines.map(_.split(' ').map(_.toLong).toVector)
    val diffs = sequences.map(sequence => calculateDiff(sequence :: Nil))
    println(diffs.map(predict(_)).sum) // 1100
  }

  @tailrec
  private def calculateDiff(differences: List[Vector[Long]]): List[Vector[Long]] = {
    val deltas = differences.head.sliding(2).map { case Seq(a, b) => b - a }.toVector
    val nextDiff = deltas :: differences
    if (deltas.forall(_ == 0)) {
      nextDiff
    } else {
      calculateDiff(nextDiff)
    }
  }

  @tailrec
  private def predict(differences: List[Vector[Long]], nextValue: Long = 0): Long = {
    val bottom = differences.head
    differences.tail.headOption match {
      case Some(top) =>
        val predictedValue = top.head - nextValue
        predict(differences.tail, predictedValue)
      case _ =>
        nextValue
    }
  }

