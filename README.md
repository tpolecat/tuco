
# tuco

<img src="tuco.jpeg" align="right" width="225" style="margin: 1px">

Reasonable telnet server for Scala.

This is a slimmed-down fork of the old [`net.wimp.telnetd`](http://telnetd.sourceforge.net/) with a pure API in the style of [doobie](https://github.com/tpolecat/doobie). It lets you embed a telnet server in your application in a reasonable way.

Why would you want to do this? Dunno. Laziness? It's easier than writing a web UI or dealing with `curl` and sometimes it's really all you need for a backdoor engineering interface.

**Tuco** lets you do arbitrary stuff, but the most common usage is to provide a set of commands with options defined via [scala-optparse-applicative](https://github.com/bmjames/scala-optparse-applicative) (included) and use the provided server implementation which provides command history, acceptable line editing, and good help facilities.

### Quick Start

**Tuco** is available for **Scala 2.10**, **2.11**, and **2.12-RC1** with

- scalaz 7.2
- scala-optparse-applicative 0.4

Add the dependency to your `build.sbt` thus:

```scala
libraryDependencies += "org.tpolecat" %% "tuco" % "0.1"
```

Construct a set of commands that you wish to provide:

```scala
// TODO
```

Construct a telnet server configured with your commands and other options:

```scala
// Use lenses to set options
val config: State[TelnetD, Unit] =
  for {
    _ <- L.commands %= (cs => cs |+| cmds)
    _ <- L.port     := 6666
    _ <- L.maxcon   := 25
  } yield ()

// Our configured telnet daemon
val t = config.exec(TelnetD.initial)
```

Run that baby.

```scala
t.start.unsafePerformIO // returns immediately; t.stop is a shutdown action
```

Try it out:

```
$ telnet localhost 6666
...
```

### Probable FAQ

- **Is this a security problem?** Yes! Telnet should definitely not be available outside your local network, and probably not even from a remote machine. It's all on you.
