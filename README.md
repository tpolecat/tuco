
## Tuco

[![Build Status](https://travis-ci.org/tpolecat/tuco.svg?branch=master)](https://travis-ci.org/tpolecat/tuco)
[![Gitter chat](https://badges.gitter.im/tpolecat/tuco.png)](https://gitter.im/tpolecat/tuco)

A reasonable telnet server for Scala.

> *"There are two kinds of spurs, my friend. Those that come in by the door … [and] those that come in by the window."*
>
> — Tuco Benedicto Pacifico Juan Maria Ramirez

Sometimes it's easier to come in by the window. **Tuco** is a slightly cleaned-up fork of the prehistoric [`net.wimpi.telnetd`](http://telnetd.sourceforge.net/) with a pure API in the style of [**doobie**](https://github.com/tpolecat/doobie). It lets you embed a telnet server in your application in a reasonable way.

### Quick Start

**Tuco** is available for **2.11**, and **2.12** with

- [cats](https://github.com/typelevel/cats) 0.9.0
- [cats-effect](https://github.com/typelevel/cats-effect) 0.3
- [decline](https://github.com/bkirwi/decline) 0.2.2

Add the dependency to your `build.sbt` thus:

```scala
libraryDependencies += "org.tpolecat" %% "tuco"       % "0.2.0" // either this one
libraryDependencies += "org.tpolecat" %% "tuco-shell" % "0.2.0" // or this one, which includes the shell API
```

Obviously this is *very early software.* Please experiment and contribute code or suggestions, but don't rely on it for anything important yet unless you are prepared to rewrite things.

Where from here? Maybe the exquisite and voluminous [**documentation**](http://tpolecat.github.io/tuco/docs/)?

### Probable FAQ

- **Is this a security problem?** Yes! Telnet should definitely not be available outside your local network, and probably not even from a remote machine. It's all on you.

- **Who the hell is Tuco Benedicto Pacifico Juan Maria Ramirez?** Please enjoy this brief [educational video](https://www.youtube.com/watch?v=p9shpHAh8uc).
