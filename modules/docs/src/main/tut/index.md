---
layout: home
title:  "Home"
section: "home"
---

[![Build Status](https://travis-ci.org/tpolecat/tuco.svg?branch=master)](https://travis-ci.org/tpolecat/tuco)
[![Gitter chat](https://badges.gitter.im/tpolecat/tuco.png)](https://gitter.im/tpolecat/tuco)

> *"There are two kinds of spurs, my friend. Those that come in by the door … [and] those that come in by the window."*
>
> — Tuco Benedicto Pacifico Juan Maria Ramirez

Sometimes it's easier to come in by the window. **Tuco** is a slightly cleaned-up fork of the prehistoric [`net.wimpi.telnetd`](http://telnetd.sourceforge.net/) with a pure API in the style of [**doobie**](https://github.com/tpolecat/doobie). It lets you embed a telnet server in your application in a reasonable way.

### Quick Start

The current development version is **{{site.tucoVersion}}** for **Scala {{site.scalaVersions}}** with

- [**cats**](http://typelevel.org/cats/) {{site.catsVersion}}
- [**cats-effect**](http://typelevel.org/cats/) {{site.catsEffectVersion}}
- [**decline**](https://github.com/bkirwi/decline) {{site.declineVersion}}
- [**monocle**](http://julien-truffaut.github.io/Monocle/) {{site.monocleVersion}}

Add the dependency to your `build.sbt` thus:

```scala
libraryDependencies += "org.tpolecat" %% "tuco-core"  % "{{site.tucoVersion}}" // either this
libraryDependencies += "org.tpolecat" %% "tuco-shell" % "{{site.tucoVersion}}" // or this, which includes the shell API
```

Obviously this is *very early software.* Please experiment and contribute code or suggestions, but don't rely on it for anything important yet unless you are prepared to rewrite things.

Where from here? Maybe the exquisite and voluminous [**documentation**](/tuco/docs/)?

### Probable FAQ

- **Is this a security problem?** Yes! Telnet should definitely not be available outside your local network, and probably not even from a remote machine. It's all on you.

- **Who the hell is Tuco Benedicto Pacifico Juan Maria Ramirez?** Please enjoy this brief [educational video](https://www.youtube.com/watch?v=p9shpHAh8uc).
