package drx.interface

import drx.concreteplatform

import scala.collection.mutable

object DSL {
  // proxy
  var innerdsl: DSLTrait = _
  def RawVar[I,O](init: Seq[I], f: Seq[I] => O): DSL.RawVar[I,O] = innerdsl.RawVar(init, f)
  def Val[X](e: => X): Val[X] = innerdsl.Val(e)

  // sugar
  type Var[X] = RawVar[X,X]
  type SeqVar[X] = RawVar[X,Seq[X]]

  def Var[X](e: => X): Var[X] = innerdsl.RawVar[X,X](Seq(e), _.last)
  def SeqVar[X](): SeqVar[X] = innerdsl.RawVar(Seq(), x => x)

  private var transactionActive = 0
  var getEnableds: mutable.Buffer[() => Set[Val[_]]] = mutable.Buffer()
  def transact[X](f: => X): X = {
    transactionActive += 1
    val x = f
    if (transactionActive == 1) {
      innerdsl.forceStep()
      concreteplatform.writeToDisk("TX")
    }
    transactionActive -= 1
    x
  }

  abstract class Val[+O](dsl: DSLTrait) {
    def sample: O

    def get: O
    def enable(): Unit
    def disable(): Unit

    def zip[Y](y: Val[Y]): Val[(O,Y)] = Val((get, y.get))
    def map[Y](f: O => Y): Val[Y] = Val(f(get))
    def foreach(f: O => Unit)(implicit owner: Owner): Val[Unit] = {
      val result = Val(f(get))
      //owner.register(result)
      result
    }
    def scan[Y](init: Y)(f: (Y, O) => Y)(implicit owner: Owner): Val[Y] = {
      var x: O = get
      var y: Y = init
      val result = Val { val newx = get; if (x != newx) { x = newx; y = f(y, x) }; y }
      //owner.register(result)
      result
    }
  }

  abstract class RawVar[I,O](reduce: Seq[I]=>O, dsl: DSLTrait) extends Val[O](dsl) {
    def set(newValue: I): Unit
    def transform(f: O => I): Unit = { val x = f(get); set(x) }
  }

  class Owner {
    val children: mutable.Buffer[Val[_]] = mutable.Buffer[Val[_]]()
    def register(valu: Val[_]): Unit = children.append(valu)
  }
}

trait DSLTrait {
  def RawVar[I,O](init: Seq[I], f: Seq[I] => O): DSL.RawVar[I,O] // events
  def Val[O](e: => O): DSL.Val[O] // signals
  protected[drx] def forceStep(): Unit
}
