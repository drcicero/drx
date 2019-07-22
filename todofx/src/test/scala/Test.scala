import org.scalacheck.Gen

object CommandsLevelDB extends org.scalacheck.Properties(name="CommandsLevelDB") {
  property("leveldbspec") = GoldCounterBisimCounter.property()
}

trait CounterI {
  def inc(x: Int): Unit
  def dec(): Unit
  def reset(): Unit
  def get: Int
}

class GoldCounter(private var n: Int) extends CounterI {
  override def inc(x: Int): Unit = n += x
  override def dec(): Unit = n -= 1
  override def reset(): Unit = n = 0
  override def get: Int = n
  override def toString: String = "GoldCounter(" + n + ")"
}

class Counter(private var n: Int) extends CounterI {
  override def inc(x: Int): Unit = if (x < 5) n += x
  override def dec(): Unit = if(n > 3) n -= 2 else n -= 1
  override def reset(): Unit = n = 0
  override def get: Int = n
}

object GoldCounterBisimCounter extends Bisimulation[Int] {
  override type Base = CounterI
  override type State = GoldCounter
  override type Sut = Counter

  override def get(b: Base): Int = b.get
  override def copy(s: State): State = new State(s.get)

  override def genInitialState: Gen[State] = Gen.const(new State(0))
  override def newSut(state: State): Sut = new Sut(state.get)

  def genCommand(state: State): Gen[Command] = Gen.oneOf(
    Gen.choose(-10, 10).map(x => cmd(_.inc(x), "inc " + x.toString)),
    Gen.const(cmd(_.dec(), "dec")),
    Gen.const(cmd(_.reset(), "reset"))
  )
}
