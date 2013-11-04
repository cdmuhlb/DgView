package cdmuhlb.dgview.spectral

import math.sqrt

object LegendrePolynomials {
  def p(order: Int, x: Double): Double = {
    require(order >= 0)
    require((x >= -1.0) && (x <= 1.0))
    val n = order.toDouble
    return JacobiPolynomials.p00(order, x)*sqrt(n + 0.5)
  }

  def d1p(order: Int, x: Double): Double = {
    require(order >= 0)
    require((x >= -1.0) && (x <= 1.0))
    val n = order.toDouble
    return JacobiPolynomials.d1p00(order, x)*sqrt(n + 0.5)
  }
}
