import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.collection.mutable

object Advent19:

  def main(args: Array[String]): Unit = {

    val Seq(workflowPart, partsPart) = Files.readString(Path.of("./inputs/input19.txt")).split("\n\n").toSeq

    val flows = workflowPart.linesIterator.toSeq.map {
      line =>
        val Seq(name, rulesLine) = line.split('{').toSeq
        val rawRules = rulesLine.split(Array(',', '}')).toSeq
        val rules = rawRules.take(rawRules.length - 1).map {
          rule =>
            val Seq(term, destination) = rule.split(':').toSeq
            val Seq(category, value) = term.split(Array('>', '<')).toSeq
            val isGreater = term.contains('>')
            Rule(category.head, isGreater, value.toInt, destination)
        }
        Flow(name, rules, rawRules.last)
    }

    val parts = partsPart.linesIterator.toSeq.map {
      line =>
        val rawCategories = line.drop(1).dropRight(1).split(',').toSeq
        Part(rawCategories.map {
          rawCategory =>
            val Seq(category, value) = rawCategory.split('=').toSeq
            (category.head, value.toInt)
        }.toMap)
    }

    //println(flows.mkString("\r\n"))
    //println(parts.mkString("\r\n"))

    val inFlow = flows.find(_.name == "in").get

    def evaluateFlow(part: Part, flow: Flow = inFlow): Boolean = {
      val destination = flow.rules.find(
        rule => rule.isGreater && part.values(rule.category) > rule.value
          || !rule.isGreater && part.values(rule.category) < rule.value) match {
        case Some(rule) => rule.destination
        case None => flow.destination
      }

      flows.find(_.name == destination) match {
        case Some(nextFlow) => evaluateFlow(part, nextFlow)
        case _ => destination == "A" // or R
      }
    }

    val acceptedParts = parts.filter(part => evaluateFlow(part))
    val acceptedSum = acceptedParts.map(_.values.values.sum).sum
    println(acceptedSum)

  }

  case class Flow(name: String, rules: Seq[Rule], destination: String)

  case class Rule(category: Char, isGreater: Boolean, value: Int, destination: String)

  case class Part(values: Map[Char, Int])