import java.nio.file.{Files, Path}
import scala.annotation.tailrec

object Advent15:

  def main(args: Array[String]): Unit = {

    val line = Files.readString(Path.of("./inputs/input15.txt")).linesIterator.toList.head
    val steps = line.split(",").toSeq

    def hash(input: String): Int = {
      var currentValue = 0
      var index = 0
      while (index < input.length) {
        currentValue = ((currentValue + input.charAt(index)) * 17) % 256
        index += 1
      }
      currentValue
    }


    println(steps.map(s => hash(s)).sum)
    //println(hash("HASH"))

  }