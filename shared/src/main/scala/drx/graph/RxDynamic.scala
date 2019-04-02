package drx.graph

import drx.internals.EmptyValExc
import drx.{atomic, debug, internals, withInstant}

import scala.collection.mutable
import scala.util.Try

/** A node in the directed graph of reactives. */
abstract private[drx] class RxDynamic[+X] private[drx](remember: Boolean,
                                                       name: String,
                                                       init: Try[X] = internals.emptyValExc(),
                                                      ) extends Getr[X] {
  //  // by giving each Node more memory, we can find memory leaks more easily
  //  private[drx] val dummy = Array.tabulate(1000 * 1000)( i => i ) // TODO

  override def toString: String = name // to find sourcefile and line numbers again

  protected[this] val formula: Try[X] => X

  private[drx] var forceObserved: Boolean = false // can be set to true to force evaluation
  protected[this] var value: Try[X] = init // contains value or exceptions calculated by formula during turns
  private[drx] def getIt: Try[X] = value

  private[drx] val outs: mutable.Set[RxDynamic[_]] = mutable.Set() // for propagation, push to these rx
  private[drx] val ins: mutable.Set[RxDynamic[_]] = mutable.Set() // for dynamic edges, remember incomings
  private[drx] var level: Int = 0 // for glitchfreedom, always higher than the all incomings level, and lower than all outgoing levels

  private[drx] def isObserved: Boolean = outs.nonEmpty || forceObserved
  private[drx] def cleanup(): Unit =
    if (!remember) value = internals.emptyValExc()

  // INVARIANT: outs         .forall(out => out.level > this.level)
  // INVARIANT: ins          .forall(in  => in.level  < this.level)
  // INVARIANT: mode==Paused ==> forall rx. !rx.out.contains(this)

  debug.debugRxs(this) = Unit // store all rxs
  private[drx] var debugEvaluating: Boolean = false // currently evaluating
  private[drx] var debugCtr: Int = 0 // how often evaluated

  @inline private[drx] def getValue: X = withInstant { tx =>
    //    if (isFrozen) {// if we are frozen, we return our immutable value
    //      println("is frozen: " + this.id); return value.get }

    if (if (remember) !isObserved && !tx.clean.contains(this)
    else value.isFailure && value.failed.get.isInstanceOf[EmptyValExc])
      tx.markRxShallow(this)

    // TODO sample should change levels, e.g. if outer==None we still need to setLevelLowerThan... Hm?

    val outer = internals.activeRx.value
    outer.foreach { outer => // if outer exists
      if (outer.isObserved) addOut(outer) // if outer is pushing (vs paused == pulling), remember to pushto outer on future changes
      this.setLevelLowerThan(outer, tx) // ensure  this.ins.level < this.level < outer.level < outer.outs.level
    }

    if (internals.withRetries) { // So now, we process any lower or equal signals than this now!
      val head = tx.dirty.peek
      if (head != null && head.level <= level) {
        // we throw an exception and mark outer to be evaluated later again,
        // the enclosing transaction will then continue evaluation.
        outer.foreach(tx.markRx)
        internals.emptyValExc().get // TODO evtl andere exception nehmen (control flow exception)
        // x emptyVal notYetVal error
      }
    } else {
      tx.processLowerOrEqual(level)
    }

    // we have ensured that the value is valid, so we can return it
    value.fold(y => internals.emptyValExc[X](y).get, y => y)
  }

  private[drx] def reeval(): Unit = {
    if (internals.DEBUGSTEP) {debug.evaluating.add(this)
      debug.writeToDisk(s"STEP $this := $value")}

    // during evaluation of formula, the 'ins' will be filled with our dependencies.
    // then, we remove ourself from those, we do not depend on anymore
    val tmpIn = Set() ++ ins; ins.clear()
    val newValue = internals.activeRx.withValue(Some(this)) { Try(formula(value)) }
    (tmpIn -- ins).foreach(_ remOut this)

    //    if (isObserved) runatleastonce = true
    //internals.activeRx.value.foreach { outer =>
    //  if (outer.getMode == Pushing) runatleastonce = true }

    // If the newValue does not signify to Abort evaluation (EmptyStream).
    val failedWithEmptyStream = newValue.failed.map(_.isInstanceOf[EmptyValExc]).getOrElse(false)
    if (!failedWithEmptyStream /* && newValue != value */) withInstant { tx =>
      value = newValue; debugCtr += 1 // save new value, increase debug change counter
      if (tx.shouldPush(this)) outs.foreach(tx.markRx) // if we are pushing, we mark all outgoings for reevaluation
    }

    if (internals.DEBUGSTEP) {println(f"$this%-20s := $newValue%-20s")
      debug.writeToDisk(s"STEP $this := $value")
      debug.evaluating.remove(this)}
  }

  private def addOut(outer: RxDynamic[_]): Unit = {
    this.outs += outer // for propagation / consistency,              remember outgoing rx
    outer.addIn(this) // for dynamic edge removal / time management, remember incoming rx
  }
  private[drx] def remOut(to: RxDynamic[_]): Unit = {
    //if (!outs.contains(to)) throw new RuntimeException(
    //  s"redundant broke from $id to ${to.id}") else
    //println(s"  broke from $id to ${to.id}")
    this.outs -= to
    // if we just lost our last outgoing, then we deactivate ourself, too.
    if (!isObserved) ins.foreach(_ remOut this)
  }
  private[drx] def addIn(value: RxDynamic[_]): Unit = ins += value

  private def setLevelLowerThan(to: RxDynamic[_], tx: atomic): Unit = {
    /** there are two kinds of strategies to handle this situation,
      * you can choose between over the global variable [[internals.withRetries]]. */
    if (internals.withRetries) to.levelup(this.level + 1, tx)
    else                     this.leveldown(to.level - 1, tx)
  }
  protected def leveldown(newLevel: Int, tx: atomic): Unit = if (newLevel < this.level) {
    level = newLevel; tx.resubmit(this) // resort on lvl change
    ins.foreach(_.leveldown(newLevel - 1, tx))
  }
  protected def levelup(newLevel: Int, tx: atomic): Unit = if (newLevel > level) {
    level = newLevel; tx.resubmit(this) // re-sort on lvl change
    outs.foreach(_.levelup(newLevel + 1, tx))
  }

  private[drx] def start(): Unit = {
    if (internals.activeRx.value.isDefined) throw new RuntimeException(
      "You are starting an foreach/scan inside the dataflow graph. " +
        "That may lead to memory leaks! Try using mkForeach/mkScan instead.")
    if (!forceObserved) {
      forceObserved = true
      withInstant(_.markRx(this))
    }
  }

  private[drx] def stop(): Unit = if (forceObserved) {
    forceObserved = false
    ins.foreach(_.remOut(this))
  }

  //  // you cannot trust ins if not run at least once
  //  private[drx] var runatleastonce: Boolean = false
  //  override protected def finalize(): Unit = println("fin " + this)
  //  // for proxyvars a pointer to yourself
  //  private[drx] def underlying: DynamicRx[X] = this
  //    private[drx] var freezer: Boolean = false
  //    override private[drx] def isFrozen: Boolean = {
  //      freezer |= isObserved && ins.isEmpty; freezer
  //    }
  //  private[drx] def freeze(): Unit = {
  //    formula = () => value.get
  //    ins.clear()
  //    runatleastonce = true
  //
  //    val tmp = Set() ++ outs; outs.clear()
  //    tmp.foreach { it => if (it.getIns.forall(_.isFrozen)) it.freeze() }
  //  }
}
