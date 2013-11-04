DgView
======

Visualize spectral element data while maintaining a responsive UI.  Expensive
rendering computations are performed by an [Akka](http://akka.io/) ActorSystem
that interfaces with `SwingWorker`.  Interpolation to pixels is done with full
spectral accuracy (this interpolation is currently unoptimized, which helps
emphazie the scalability of the actor-based rendering framework).

Dependencies
------------

_DgView_ is written in Scala and requires an
[SBT](http://www.scala-sbt.org/) launcher compatible with version 0.13.0 and a
Java SE 6 JVM.

Configuration
-------------

Options not hard-coded into the application can be configured in
`application.conf`.
