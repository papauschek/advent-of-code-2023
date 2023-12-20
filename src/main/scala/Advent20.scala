import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.collection.mutable

object Advent20:

  def main(args: Array[String]): Unit = {

    val lines = Files.readString(Path.of("./inputs/input20.txt")).linesIterator.toSeq

    val modules = lines.map {
      line =>
        val Array(rawName, outputs) = line.split(" -> ")
        val name = if (rawName == "broadcaster") rawName else rawName.substring(1)
        Module(name, rawName.head, outputs.split(", ").toSeq)
    }

    //println(modules.mkString("\r\n"))

    val outputModules = modules.flatMap(_.outputs).distinct.filter(name => !modules.exists(_.name == name))
    val outputModuleStates = outputModules.map(name => ModuleState(Module(name, 'o', Seq.empty)))
    val states = modules.map(m => ModuleState(m, Seq.empty, Seq.empty)) ++ outputModuleStates
    states.foreach {
      state =>
        state.inputModules = states.filter(s => s.module.outputs.contains(state.module.name))
        state.outputModules = state.module.outputs.map(outputName => states.find(_.module.name == outputName).getOrElse(
          throw new Exception(s"Could not find module $outputName")))
    }

    val buttonState = ModuleState(Module("button", ' ', Seq.empty))
    val broadcasterState = states.find(_.module.moduleType == 'b').get

    var (lowPulseCount, highPulseCount) = (0, 0)
    var wasReceived = false
    (0 until 1000).foreach {
      _ =>
        var pulses = Vector(Pulse(buttonState, broadcasterState, false))
        while (pulses.nonEmpty) {
          val pulse = pulses.head
          if (pulse.value) highPulseCount += 1 else lowPulseCount += 1
          val newPulses = pulse.destination.receivePulse(pulse.source, pulse.value)
          pulses = pulses.tail ++ newPulses
        }
    }

    //println(states.mkString("\r\n"))
    println((lowPulseCount, highPulseCount, lowPulseCount * highPulseCount))

  }

  case class Pulse(source: ModuleState, destination: ModuleState, value: Boolean)

  case class Module(name: String, moduleType: Char, outputs: Seq[String])

  class ModuleState(val module: Module,
                    var inputModules: Seq[ModuleState] = Seq.empty,
                    var inputs: Seq[Boolean] = Seq.empty,
                    var isOn: Boolean = false, // flip flops
                    var outputModules: Seq[ModuleState] = Seq.empty) {

    override def toString: String = s"ModuleState($module, ${inputModules.map(_.module.name).mkString(", ")})"

    def receivePulse(source: ModuleState, value: Boolean): Seq[Pulse] = {

      val pulses = module.moduleType match {
        case 'b' => outputModules.map(Pulse(this, _, value))
        case '%' =>
          if (value) {
            Nil // ignore
          } else {
            isOn = !isOn // flip flop
            outputModules.map(Pulse(this, _, isOn))
          }
        case '&' =>
          val inputIndex = inputModules.indexOf(source)
          if (inputs.isEmpty) inputs = Seq.fill(inputModules.size)(false)
          inputs = inputs.updated(inputIndex, value)
          if (inputs.forall(identity)) {
            outputModules.map(Pulse(this, _, false))
          } else {
            outputModules.map(Pulse(this, _, true))
          }
        case _ => Nil
      }

      println(s"${pulses.map(pulse => s"${pulse.source.module.name} ${pulse.value} ${pulse.destination.module.name}").mkString("\r\n")}")
      pulses
    }

  }