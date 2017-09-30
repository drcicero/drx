# drx - dynamic reactive programming

*Reactive Programming without (memory) leaks is possible.*

drx is a library for reactive programming with Scala and Scala.js, without -- namely reactives that are created at runtime and safely collected by a garbage collector, if no longer used anywhere.

Goal: We want to be able to make the whole handling of Variables, Signals and Observers leakfree, with restricting first-classness of things as the last resort.

Features:

* dynamic edges / dependency tracking: 
* dynamic signals / first-class signals: Signals can be created at runtime, and even inside Signals.
* --> Dynamic Edges and Signals go hand-in-hand to allow basic support for incremental changes.
* folds / ad-hoc internal state: Sometimes you just need state, a counter for example cannot count without access to its previous value. State is important, however we restrict creation of folds outside of signals, because folds cannot be set to sleep.

* signals (map, fold, get), variables (set), observer

* Only signals, that are observed need to run.

* We extend the notion of sleeping to observers themselves. For example in the case of GUIs observers are not visible on screen dont need to run.

* Not running signals, do not need to reference their push-to-targets. When we remove this references, a garbage collector can collect them, if no longer referenced anywhere else. The idea is from Petricek et al. [1].

* To prevent glitches (simplified: running reactives twice for a single change) libraries often evaluate reactives in topological order. The topological order is tracked at runtime, by assigning each reactive a height one higher than their predecessors. Our algorithm instead assignes each reactives a negative height one lower than ints successors.

* We implement two versions of a simple application to create, edit and delete Todos, one running on JS and one for the JVM.

Run the web example:
~~~
sbt todojs/fastOptJS # compile
firefox index.html   # open app in browser
~~~

Note, that with some browsers you can not open local web pages. In this case, you can use python to serve the page to localhost:
~~~
python3 -m http.server 8000
chromium-browser "http://localhost:8000"
~~~

Run the JavaFX example:
~~~
# ... first get jfxrt.jar somewhere ...!
sbt todofx/run
~~~

[1] Petricek, Tomas, and Don Syme. "Collecting hollywood's garbage: avoiding space-leaks in composite events." ACM Sigplan Notices 45, no. 8 (2010): 53-62.
