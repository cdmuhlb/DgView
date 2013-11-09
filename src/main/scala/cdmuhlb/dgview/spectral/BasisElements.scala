package cdmuhlb.dgview.spectral

import scala.annotation.tailrec
import math.{Pi, abs, cos}

case class LegendreGauss(nNodes: Int) extends LegendreElement {
  require(nNodes >= 1)
  val nMax = nNodes - 1
  val nodes = LegendreElement.gaussNodes(nNodes)
  val weights = LegendreElement.gaussWeights(nodes)
}

case class LegendreGaussLobatto(nNodes: Int) extends LegendreElement {
  require(nNodes >= 2)
  val nMax = nNodes - 2
  val nodes = LegendreElement.gaussLobattoNodes(nNodes)
  val weights = LegendreElement.gaussLobattoWeights(nodes)

  def cardinal(i: Int, x: Double): Double = {
    require((i >= 0) && (i < nNodes))
    require((x >= -1.0) && (x <= 1.0))
    val x0 = nodes(i)
    if (x == x0) 1.0
    else {
      val n = nNodes.toDouble
      (x*x - 1.0)*JacobiPolynomials.d1p00(nNodes - 1, x) /
          (n*(n - 1.0)*(x - x0)*JacobiPolynomials.p00(nNodes - 1, x0))
    }
  }
}

trait QuadratureElement1D {
  def nNodes: Int
  def nodes: Vector[Double]
  def weights: Vector[Double]

  def integrate(y: Vector[Double]): Double = {
    require(y.length == nNodes)
    y.zip(weights).map(yw ⇒ yw._1*yw._2).sum
  }
}

trait LegendreElement extends QuadratureElement1D {
  def nMax: Int
  def basis(n: Int, x: Double): Double = {
    require((n >= 0) && (n <= nMax))
    require((x >= -1.0) && (x <= 1.0))
    LegendrePolynomials.p(n, x)
  }

  def interpolationMatrix(x: Vector[Double]): Vector[Vector[Double]] = {
    val ans = Array.ofDim[Double](x.length, nNodes)
    for (row <- 0 until x.length) {
      assert((x(row) >= -1.0) && (x(row) <= 1.0))
      // from SpEC:Utils/Math/InterpolationWeights.cpp
      var c1 = 1.0
      var c4 = nodes(0) - x(row)
      ans(row)(0) = 1.0
      for (i <- 1 until nNodes) {
        var c2 = 1.0
        val c5 = c4
        c4 = nodes(i) - x(row)
        for (j <- 0 until i) {
          val c3 = nodes(i) - nodes(j)
          c2 *= c3
          if (j == i-1) ans(row)(i) = -c1*c5*ans(row)(i-1)/c2
          ans(row)(j) = c4*ans(row)(j)/c3
        }
        c1 = c2
      }
    }
    ans.map(_.toVector).toVector
  }
}

object LegendreElement {
  def gaussNodes(n: Int): Vector[Double] = {
    val solver = new NewtonSolver((x: Double) ⇒ JacobiPolynomials.p00(n, x),
        (x: Double) ⇒ JacobiPolynomials.d1p00(n, x))
    val nodes = Array.ofDim[Double](n)
    for (i ← 0 until (n/2)) {
      // See Numerical Recipes, Third Edition, p. 184
      val xGuess = -cos(Pi*(i + 0.75)/(n + 0.5))
      nodes(i) = solver.findRoot(xGuess)
      nodes(n-i-1) = -nodes(i)
    }
    if (n%2 != 0) nodes(n/2) = 0.0
    nodes.toVector
  }

   def gaussWeights(nodes: Vector[Double]): Vector[Double] = {
    val n = nodes.length
    nodes.map(x ⇒ {
      val dp = JacobiPolynomials.d1p00(n, x)
      2.0 / ((1.0 - x*x)*dp*dp)
    })
  }

  def gaussLobattoNodes(n: Int): Vector[Double] = {
    val solver = new NewtonSolver((x: Double) ⇒ JacobiPolynomials.p11(n - 2, x),
        (x: Double) ⇒ JacobiPolynomials.d1p11(n - 2, x))
    val nodes = Array.ofDim[Double](n)
    val brackets = gaussNodes(n - 1)
    for (i ← 1 until (n/2)) {
      val xGuess = 0.5*(brackets(i-1) + brackets(i))
      nodes(i) = solver.findRoot(xGuess)
      nodes(n-i-1) = -nodes(i)
      assert((nodes(i) > brackets(i-1)) && (nodes(i) < brackets(i)))
    }
    if (n%2 != 0) nodes(n/2) = 0.0
    nodes(0) = -1.0
    nodes(n-1) = 1.0
    nodes.toVector
  }

  def gaussLobattoWeights(nodes: Vector[Double]): Vector[Double] = {
    val n = nodes.length
    nodes.map(x ⇒ {
      val p = JacobiPolynomials.p00(n - 1, x)
      2.0 / (n*(n - 1.0)*p*p)
    })
  }
}

class NewtonSolver(f: (Double) ⇒ Double, df: (Double) ⇒ Double) {
  def findRoot(x0: Double): Double = {
    @tailrec
    def newton(x: Double): Double = {
      import java.lang.Double.MIN_NORMAL
      val xNext = x - f(x)/df(x)
      // Note: Tolerance limit: MIN_VALUE/MIN_NORMAL=2.220446049250313E-16
      if ((abs(x - xNext) / abs(xNext).max(MIN_NORMAL)) < 1.0e-15) {
        xNext
      } else newton(xNext)
    }
    newton(x0)
  }
}
