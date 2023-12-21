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

    def evaluateFlowByName(part: Part, flowName: String): Long = {
      flows.find(_.name == flowName) match {
        case Some(flow) => evaluateFlow(part, flow)
        case _ if flowName == "A" => part.combinations
        case _ if flowName == "R" => 0
        case _ => throw new Exception(s"unknown flow $flowName")
      }
    }

    def evaluateFlow(part: Part, flow: Flow): Long = {
      var remainingPart = part
      var acceptedCount = 0L
      flow.rules.foreach {
        rule =>
          val (included, excluded) = remainingPart.partition(rule.category, rule.isGreater, rule.value)
          acceptedCount += evaluateFlowByName(included, rule.destination)
          remainingPart = excluded
      }
      evaluateFlowByName(remainingPart, flow.destination) + acceptedCount
    }

    val fullRange = Range(1, 4000)
    val ratings = Seq('x', 'm', 'a', 's')
    val initialPart = Part(ratings.map(r => (r, fullRange)).toMap)

    val acceptedCount = evaluateFlowByName(initialPart, flowName = "in")
    println(acceptedCount) // 127517902575337
  }


  case class Flow(name: String, rules: Seq[Rule], destination: String)

  case class Rule(category: Char, isGreater: Boolean, value: Int, destination: String)

  case class Part(values: Map[Char, Range]) {

    def partition(category: Char, isGreater: Boolean, value: Int): (Part, Part) = {
      val (included, excluded) = if (isGreater) {
        values(category).greaterThan(value)
      } else {
        values(category).smallerThan(value)
      }
      (Part(values.updated(category, included)), Part(values.updated(category, excluded)))
    }

    def isEmpty: Boolean = values.values.exists(_.isEmpty)

    def combinations: Long = {
      values.values.map(_.count.toLong).product
    }

    override def toString: String = s"{${values.map((k, v) => s"$k=$v").mkString(", ")}}"

  }

  case class Range(start: Int, count: Int) {
    
    val end: Int = start + count
    
    /** return intersection and remainder */
    def smallerThan(max: Int): (Range, Range) = {
      if (end <= max) (this, Range(0, 0))
      else (Range(start, max - start), Range(max, end - max))
    }

    def greaterThan(min: Int): (Range, Range) = {
      if (start > min) (this, Range(0, 0))
      else (Range(min + 1, end - min - 1), Range(start, min - start + 1))
    }

    def isEmpty: Boolean = count <= 0

    override def toString: String = s"[$start, $end)"
    
  }

