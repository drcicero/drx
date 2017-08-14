# drx - dynamic reactive programming

An experiment in scala to add safe automatic memory management of dynamic reactives to reactive programming -- namely reactives that are created at runtime and safely collected by a garbage collector, if no longer used anywhere. The basic concept -- setting unneeded signals to sleep and waking them up when needed -- is based on [1]. Additionaly we set observers to sleep and wake them up if their result is not visible on screen. Contains as an example Todo-Management-App.

Run the example:
~~~
sbt todo/fastOptJS
firefox index.html
~~~

Or:
~~~
sbt todo/fastOptJS
chromium-browser "http://localhost:8000" & python3 -m http.server 8000
~~~

[1] Petricek, Tomas, and Don Syme. "Collecting hollywood's garbage: avoiding space-leaks in composite events." ACM Sigplan Notices 45, no. 8 (2010): 53-62.
