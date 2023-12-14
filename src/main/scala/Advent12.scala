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

    def countSolutionsFast(data: String,
                           damagedCounts: List[Int],
                           nextIndex: Int = 0,
                           nextDamagedCount: Int = 0): Long = {

      val nextData = data

      /*
      if (nextIndex > 0) {
        if (nextDamagedCount > 0) nextData = data.updated(nextIndex - 1, '#')
        else nextData = data.updated(nextIndex - 1, '.')
      }
      */

      var index = nextIndex
      var damagedCount = nextDamagedCount
      var remainingCounts = damagedCounts
      var continue = true
      while (index < data.length && continue) {
        val char = data(index)
        if (char == '#') {
          damagedCount += 1
          if (remainingCounts.isEmpty || remainingCounts.head < damagedCount) {
            index = data.length // premature end
            continue = false // break. pattern damages are too long to match the recorded damages
          }
        } else if (char == '.') {
          if (damagedCount > 0) {
            if (remainingCounts.isEmpty || remainingCounts.head != damagedCount) {
              index = data.length // premature end
              continue = false // break. pattern damages are too long to match the recorded damages
            } else {
              damagedCount = 0
              remainingCounts = remainingCounts.tail
            }
          }
        } else if (char == '?') {
          continue = false // break
        }

        if (continue) index += 1
      }

      if (index == data.length) {
        // reached the end
        if (damagedCount == 0) {
          if (remainingCounts.isEmpty) {
            //println(nextData)
            1 // valid solution
          } else {
            0 // invalid solution. pattern damages are too short to match the recorded damages
          }
        } else if (remainingCounts.isEmpty || remainingCounts.head != damagedCount || remainingCounts.length >= 2) {
          0 // invalid solution. pattern damages are too long to match the recorded damages
        } else {
          //println(nextData)
          1 // valid solution. pattern end matches the recorded damage
        }
      } else {
        // arrived at next '?'
        if (damagedCount > 0 && (remainingCounts.isEmpty || remainingCounts.head < damagedCount)) {
          0 // invalid solution. pattern damages are too long to match the recorded damages
        } else {
          if (damagedCount > 0) {
            if (remainingCounts.head == damagedCount) {
              // process last damage count, but forced to stop the chain of damages
              countSolutionsFast(nextData, remainingCounts.tail, index + 1)
            } else {
              // remainingCounts.head > damagedCount: chain of damages must continue to reach the expected count
              countSolutionsFast(nextData, remainingCounts, index + 1, nextDamagedCount = damagedCount + 1)
            }
          } else {
            val solutionsWithDamage = countSolutionsFast(nextData, remainingCounts, index + 1, nextDamagedCount = 1)
            val solutionsWithOperational = countSolutionsFast(nextData, remainingCounts, index + 1)
            solutionsWithDamage + solutionsWithOperational
          }
        }
      }
    }

    //println(rows.map(row => countSolutions(row)).mkString("\r\n"))
    //println(rows.map(row => countSolutions(row)).sum)

    //println(countSolutions(Row("???.###", List(1, 1, 3))))
    //println(countSolutionsFast("???.###", List(1, 1, 3)))
    //println(rows.map(row => countSolutionsFast(row.data, row.damages.toList)).mkString("\r\n"))

    println(expandedRows.zipWithIndex.map {
      (row, index) =>
        val count = countSolutionsFast(row.data, row.damages.toList)
        println(index.toString + " " + row.data + ", count = " + count.toString)
        count
    }.sum)

  }

  case class Row(data: String, damages: Seq[Int])


