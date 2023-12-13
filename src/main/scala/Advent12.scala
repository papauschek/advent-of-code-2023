import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder

object Advent12:

  case class Point(x: Long, y: Long)

  def main(args: Array[String]): Unit = {

    val lines = Files.readString(Path.of("./inputs/input12.txt")).linesIterator.toSeq
    val rows = lines.map {
      line =>
        val Seq(data, rawDamages) = line.split(' ').toSeq
        Row(data, rawDamages.split(',').map(_.toInt).toSeq)
    }

    val expandedRows = rows.map {
      row =>
        val newData = (1 to 5).map(_ => row.data).mkString("?")
        val newDamages = (1 to 5).flatMap(_ => row.damages)
        Row(newData, newDamages)
    }

    //println(expandedRows.mkString("\r\n"))

    def getDamagedCounts(data: String): Vector[Int] = {
      var index = 0
      var damaged = false
      var damagedStartIndex = 0
      val damagedCounts = new VectorBuilder[Int]
      while (index < data.length) {
        val char = data(index)
        if (char == '#') {
          if (!damaged) {
            damagedStartIndex = index
            damaged = true
          }
          if (index == data.length - 1) {
            damagedCounts += index - damagedStartIndex + 1
          }
        } else if (char == '.' && damaged) {
          damagedCounts += index - damagedStartIndex
          damaged = false
        } else if (char == '?') {
          index = data.length // break
        }
        index += 1
      }
      damagedCounts.result()
    }

    def countSolutions(row: Row): Int = {
      val damagedCounts = getDamagedCounts(row.data)

      if (damagedCounts.length <= row.damages.length) {

        var equalCount = 0
        while (equalCount < damagedCounts.length && damagedCounts(equalCount) == row.damages(equalCount)) {
          equalCount += 1
        }

        if (equalCount == damagedCounts.length) {
          row.data.indexOf('?') match {
            case -1 =>
              val isEquals = equalCount == row.damages.length
              //if (isEquals) println(row.data + " " + row.damages.mkString(","))
              if (isEquals) 1 else 0
            case index =>
              val operationalCount = countSolutions(row.copy(data = row.data.updated(index, '.')))
              val damagedCount = countSolutions(row.copy(data = row.data.updated(index, '#')))
              operationalCount + damagedCount
          }
        } else {
          0 // pattern can no longer match the recorded damages
        }
      } else {
        0 // pattern damages are too long to match the recorded damages
      }
    }

    //println(rows.map(row => countSolutions(row)).mkString("\r\n"))
    println(rows.map(row => countSolutions(row)).sum)

    println(expandedRows.map {
      row =>
        val count = countSolutions(row)
        println("count " + count.toString)
        count
    }.sum)

  }

  case class Row(data: String, damages: Seq[Int])


