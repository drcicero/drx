/** this package implements dynamic exprs (Scan, Val) */
package drx

import drx.graph.{RxDynamic, Rx}

import scala.util.{DynamicVariable, Failure, Success, Try}

//pipe iter(1, 2, 3, 4, 5)
//     map(x => x+1)
//     filter(x => x % 2)
//     println
//
//do  x <- 1, 2, 3, 4, 5
//    x <- x+1
//    if x % 2
//    println

//trait ToRx[X] extends InstantRx_[X] {
//  protected def levelup(newLevel: Int, tx: Instant): Unit
////  private[drx] def getIns: Set[FromRx[_]] // freeze
////  private[drx] def freeze(): Unit // freeze
//}
//trait OuterRx[X] extends ToRx[X] {
//  private[drx] def isObserved: Boolean
//  private[drx] def addIn(value: FromRx[_]): Unit
//}
//trait FromRx[X] {
//  protected def leveldown(newLevel: Int, tx: Instant): Unit
//  private[drx] def remOut(to: ToRx[_]): Unit
//  //  private[drx] def isFrozen: Boolean // freeze
//}

// Getr --> Obsable


private object internals {
  class EmptyValExc(cause: Throwable) extends Throwable(cause)

  @inline def emptyValExc[X](e:Throwable = null): Try[X] = Failure {
    val result = new EmptyValExc(e)
    val index: Int = result.getStackTrace.lastIndexWhere(x =>
      x.getClassName.startsWith("Rendering") || x.getClassName.startsWith("drx.")
    )
    result.setStackTrace(result.getStackTrace.slice(index + 1, index + 2))
    result
  }

  // there are two kinds of strategies to handle dynamic edges
  val withRetries = false // true = old variant, false = cooler variant
  // TODO: NEW VARIANT: COUNT INCOMING

  val activeRx: DynamicVariable[Option[RxDynamic[_]]] = new DynamicVariable(None)

//  var uniqueCtr = 0
//  def count: Int = { uniqueCtr += 1; uniqueCtr }
  val DEBUGSTEP = false

}

//  val strongEventsources: mutable.Set[InternalRx[_]] = mutable.Set()
//  val dummyOwner = new VarOwner {}
//  def checksafety(me: InternalRx[_], owner: VarLike): Unit = {
//    val ancestry: mutable.Set[InternalRx[_]] = mutable.Set()
//    def transitiveIns(it: InternalRx[_]): Unit = {
//      if (ancestry.add(it)) it.getIns.foreach(transitiveIns)
//    }
//    transitiveIns(me)
//    val vars = ancestry.collect { case it: EventSource[_] => it }
//    if (vars.size > 1 && !vars.subsetOf(owner.getEventsources))
//      throw new RuntimeException("You are creating a fold over multiple variables." +
//        " The fold will live until all incoming variables are dead.." +
//        " You must specify the VarOwner of all Variables that influence this fold." +
//        " for example like: a = owner.mkVar(); b = owner.mkVar() _.fold(0, owner)(_+_) ." +
//        " The fold will be then be collected when this VarOwner is collected.")
//  }
