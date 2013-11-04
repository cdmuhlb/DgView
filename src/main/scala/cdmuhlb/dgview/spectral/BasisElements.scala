package cdmuhlb.dgview.spectral

import scala.annotation.tailrec
import math.{Pi, abs, cos}

case class LegendreGaussLobatto(nNodes: Int) extends LegendreElement {
  require(nNodes >= 2)
  val nMax = nNodes - 2
  val nodes = LegendreElement.gaussLobattoNodes(nNodes)

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
}

trait LegendreElement extends QuadratureElement1D {
  def nMax: Int
  def basis(n: Int, x: Double): Double = {
    require((n >= 0) && (n <= nMax))
    require((x >= -1.0) && (x <= 1.0))
    LegendrePolynomials.p(n, x)
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
}

class NewtonSolver(f: (Double) ⇒ Double, df: (Double) ⇒ Double) {
  def findRoot(x0: Double): Double = {
    @tailrec
    def newton(x: Double): Double = {
      import java.lang.Double.MIN_NORMAL
      val xNext = x - f(x)/df(x)
      // Note: Tolerance limit: MIN_VALUE/MIN_NORMAL=2.220446049250313E-16
      if ((abs(x - xNext) / abs(xNext).max(MIN_NORMAL)) < 5.0e-16) {
        xNext
      } else newton(xNext)
    }
    newton(x0)
  }
}
