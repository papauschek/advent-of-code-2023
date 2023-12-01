import java.nio.file.{Files, Path}

@main
def main(): Unit = {

  val lines = Files.readString(Path.of("./inputs/input1.txt")).linesIterator.toSeq

  // as words are sometimes overlapping (e.g. "twone"),
  // we are keeping the first and last letters to allow multiple replacements
  val words = Seq(
    "one" -> "o1ne",
    "two" -> "t2wo",
    "three" -> "t3hree",
    "four" -> "f4our",
    "five" -> "f5ive",
    "six" -> "s6ix",
    "seven" -> "s7even",
    "eight" -> "e8ight",
    "nine" -> "n9ine"
  )

  // replace all written numbers with digits
  val digitizedLines = lines.map {
    line =>
      words.foldLeft(line) {
        case (line, (word, digit)) => line.replaceAll(word, digit)
      }
  }

  def getValue(line: String): Long = {
    val digit1 = line.find(_.isDigit).get.asDigit
    val digit2 = line.findLast(_.isDigit).get.asDigit
    digit1 * 10 + digit2
  }

  //println(lines.zip(digitizedLines).mkString("\r\n"))
  println(digitizedLines.map(getValue).sum) // 53615
}
