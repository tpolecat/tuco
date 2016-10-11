
# tuco

<img src="tuco.jpeg" align="right" width="225" style="margin: 1px">

Reasonable telnet server for Scala.

This is a slightly cleaned-up fork of the prehistoric [`net.wimpi.telnetd`](http://telnetd.sourceforge.net/) with a pure API in the style of [doobie](https://github.com/tpolecat/doobie). It lets you embed a telnet server in your application in a reasonable way.

Why would you want to do this? Dunno. Laziness? It's easier than writing a web UI or dealing with `curl` and sometimes it's really all you need for a backdoor engineering interface.

**Tuco** lets you do arbitrary stuff, but the most common usage is to provide a set of commands with options defined via [scala-optparse-applicative](https://github.com/bmjames/scala-optparse-applicative) (included) and use the provided `CommandShell` which provides command history, okay line editing, and good help facilities.

### Quick Start

**Tuco** is available for **Scala 2.10** and **2.11** with

- scalaz 7.2
- scala-optparse-applicative 0.4

Add the dependency to your `build.sbt` thus:

```scala
libraryDependencies += "org.tpolecat" %% "tuco" % "0.1-SNAPSHOT"
```

See `example.scala` for now (hit `t` to find it). TODO: tut doc

### Probable FAQ

- **Is this a security problem?** Yes! Telnet should definitely not be available outside your local network, and probably not even from a remote machine. It's all on you.
