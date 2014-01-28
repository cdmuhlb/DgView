DgView
======

Visualize spectral element data while maintaining a responsive UI.  Expensive
rendering computations are performed by an [Akka](http://akka.io/) ActorSystem
that interfaces with SwingWorker.  Interpolation to pixels is done with full
spectral accuracy.

Dependencies
------------

_DgView_ is written in Scala and requires an
[SBT](http://www.scala-sbt.org/) launcher compatible with version 0.13.0 and a
Java SE 6 (or later) JDK.

_DgView_ can generate scripts to render animations of your data in both GIF and
HTML5 video formats.  Generating GIFs requires the `convert` tool from
ImageMagick.  Generating HTML5 video requires `ffmpeg`, `x264`, `MP4Box`, and
`vpxenc`, in addition to a `bash` shell.

Configuration
-------------

Options not hard-coded into the application (including the path to the domain
file to plot) are specified in `application.conf`.

The use of JNI by the "netlib-java" dependency can cause portability issues.  If
you encounter JNI errors, try commenting or uncommenting the
`com.github.fommil.netlib` options.

Distribution
------------

_DgView_ uses the
[Native Packager Plugin](http://www.scala-sbt.org/sbt-native-packager/) for SBT
in order to create distributable binaries that only depend on a JVM (SBT and a
full JDK are not required).  To create such a package, run
`sbt universal:package-bin`; this will create a file named
`target/universal/dgview-1.0-SNAPSHOT.zip`, which can be extracted on the target
system.  Running `bin/dgview` will execute _DgView_.

Data format
-----------

_DgView_ currently reads its data from an ad hoc plain-text file format.  Domains
must be composed of rectangular elements using a basis of Legendre polynomials.
These elements may be arbitrarily scaled and shifted and need not conform to
their neighbors, but they may not be rotated or otherwise have their coordinates
mapped.

Each timestep must be stored in a separate file, and the files must be named
`VarsTime<step_num>.data`, where `<step_num>` is an integer representing the
the timestep of the data.  These files must be located in the same directory.
For example, the directory `/path/to/run/data` might contain the files
`VarsTime0.data`, `VarsTime100.data`, etc..  One could then specify
`dgview.domain-dir=/path/to/run/data` in `application.conf`.

The top of each file consists of a header indicating the fields present in the
data.  Each line of the header begins with a number sign `#` followed by a
space, followed by the column number in square brackets (starting with `1`),
followed by a space, followed by the field name (an arbitrary string).  The
first two fields must correspond to the x and y coordinates of a gridpoint.
An example header might look like:

    # [1] X[0]
    # [2] X[1]
    # [3] rho
    # [4] Pressure

Following the header is the data, one line per gridpoint.  Each line is divided
into columns by whitespace.  Each column contains a floating-point number
representing the corresponding field in the header.  Data points appear in
row-major order: left-to-right, top-to-bottom.  Each row of an element (constant
y-value) must be separated by a blank line.  Each element must be separated by
two blank lines.  An example consisting of two 2x3 elements might look like:

    0.0  0.0  1.0  2.0
    0.5  0.0  2.0  8.0
    
    0.0  0.5  1.0  2.0
    0.5  0.5  2.0  8.0
    
    0.0  1.0  1.0  2.0
    0.5  1.0  2.0  8.0
    
    
    0.5  0.0  2.0  8.0
    1.0  0.0  3.0  18.0
    
    0.5  0.5  2.0  8.0
    1.0  0.5  3.0  18.0
    
    0.5  1.0  2.0  8.0
    1.0  1.0  3.0  18.0
    
    
