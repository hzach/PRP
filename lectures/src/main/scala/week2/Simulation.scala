package week2

/**
 * Created by zach on 4/20/15.
 */

trait Simulation {

  type Action = () => Unit

  case class Event(time: Int, action: Action)

  private type Agenda = List[Event]

  private var agenda: Agenda = List()

  private var curtime = 0

  def currentTime: Int = curtime

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event( currentTime + delay,  () => block)
    agenda = insert(agenda, item)
  }

  private def insert(ag: List[Event], item: Event): List[Event] = ag match {
    case first::rest if first.time <= item.time =>
      first :: insert(rest, item)
    case _ =>
      item :: ag
  }

  /**
   * The event handling loop removes successive elements from the agenda,
   * and performs the associated actions. */
  private def loop(): Unit = agenda match {
    case first::rest =>
      agenda = rest
      curtime = first.time
      first.action()
      loop()
    case Nil =>
  }

  def run(): Unit = {
    afterDelay(0) {
      println("*** simulation started, time = " + currentTime + " ***")
    }
    loop()
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $currentTime vale = ${wire.getSignal}")
    }
  }
}

trait Parameters {
  def InverterDelay = 2
  def AndGateDelay = 3
  def OrGateDelay = 5
}

class Wire extends Simulation {

  /** represents the current value of the signal */
  private var sigVal = false
  /** represents that actions currently attached to the wire */
  private var actions: List[Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean): Unit =
    if (s != sigVal) {
      sigVal = s
      actions foreach (_())
    }

  def addAction(a: Action): Unit = {
    actions = a :: actions
    a()
  }

}

abstract class Gates extends Simulation {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def inverter(input: Wire, output: Wire): Unit = {

    def invertAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {
        output setSignal !inputSig
      }
    }

    input addAction invertAction
  }

  def andGate(in1: Wire, in2: Wire, out: Wire): Unit = {
    def andAction():Unit = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(AndGateDelay) {
        out setSignal in1Sig && in2Sig
      }
    }
    in1 addAction andAction
    in2 addAction andAction
  }

  def orGate(in1: Wire, in2: Wire, out: Wire): Unit = {
    def orAction(): Unit = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(OrGateDelay) {
        out setSignal in1Sig | in2Sig
      }
    }
    in1 addAction orAction
    in2 addAction orAction
  }

}

abstract class Circuits extends Gates {

  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit = {
    val d, e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire): Unit = {
    val s, c1, c2 = new Wire
    halfAdder(a, cin, s, c1)
    halfAdder(b, s, sum, c2)
    orGate(c1, c2, cout)
  }
}



