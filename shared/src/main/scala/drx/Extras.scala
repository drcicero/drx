package drx

object Extras {
  def lazyExtensionAttr[X,Y](func: (X) => Y): (X) => Y = {
    val cache = new platform.platform.WeakMap[X, Y]()
    (x: X) => cache.get(x).getOrElse(internals.activeRx.withValue(None) {
      val tmp = func(x)
      cache.set(x, tmp)
      tmp
    })
  }

  def zip[X,Y](rx1: Rx[X], rx2: Rx[Y]): Rx[(X,Y)] =
    Signal((rx1.abstractget, rx2.abstractget))

//  implicit class RxRx[X](rxrx: Rx[Rx[X]]) {
//    def flattenChanges(name: String = ""): Rx[X] =
//      Signal(rxrx.get.changes().get)
}

//    {
//      // TODO static flattening prob does not work as this:
//      val result = new Rx[X](StreamKind, name)
//      result.formula = () => result.father.get.get
//      rxrx.map { rx =>
//        result.father.foreach(_.unpushto(result))
//        result.father = Some(rx)
//      }
//      result
//    }
