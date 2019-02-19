package drx

object Extras {
  /* allow folds */
  def lazyExtAttr[X,Y](func: X => Y, name: String=""): X => Y = {
    val cache = new platform.platform.WeakMap[X, Y]()

    x: X => {
//      var neednew = ""
      val tmp = cache.get(x).getOrElse(internals.activeRx.withValue(None) {
//        neednew = " (new)"
        val tmp = func(x)
        cache.set(x, tmp)
        tmp
      })
//      println("try " + name +"-"+ cache.hashCode +" ["+ x.getClass +"@"+ x.hashCode + "] = " + tmp.getClass + "@" + tmp.hashCode +" "+ tmp + neednew)
      tmp
    }
  }

//  implicit class RxRx[X](rxrx: Rx[Rx[X]]) {
//    def flattenChanges(name: String = ""): Rx[X] =
//      Signal(rxrx.get.changes().get)
}

//    {
//      // TODO static flattening prob does not work as this:
//      val result = new Rx[X](name)
//      result.formula = () => result.father.get.get
//      rxrx.map { rx =>
//        result.father.foreach(_.unpushto(result))
//        result.father = Some(rx)
//      }
//      result
//    }
