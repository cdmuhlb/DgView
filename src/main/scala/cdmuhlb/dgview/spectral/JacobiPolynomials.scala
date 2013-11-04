package cdmuhlb.dgview.spectral

object JacobiPolynomials {
  def p00(order: Int, x: Double): Double = {
    assert(order >= 0)
    assert((x >= -1.0) && (x <= 1.0))
    var p_n0 = 0.0
    var p_n1 = 1.0
    var n1 = 0.0
    for (ord ← 1 to order) {
      val p_n2 = (x*p_n1*(2.0*n1 + 1.0) - p_n0*n1) / (n1 + 1.0)
      p_n0 = p_n1
      p_n1 = p_n2
      n1 += 1.0
    }
    p_n1
  }

  def p11(order: Int, x: Double): Double = {
    assert(order >= 0)
    assert((x >= -1.0) && (x <= 1.0))
    var p_n0 = 0.0
    var p_n1 = 1.0
    var n1 = 0.0
    for (ord ← 1 to order) {
      val p_n2 = (x*p_n1*(2.0*n1 + 3.0)/(n1 + 1.0) - p_n0) *
                 (n1 + 2.0)/(n1 + 3.0)
      p_n0 = p_n1
      p_n1 = p_n2
      n1 += 1.0
    }
    p_n1
  }

  def p22(order: Int, x: Double): Double = {
    assert(order >= 0)
    assert((x >= -1.0) && (x <= 1.0))
    var p_n0 = 0.0
    var p_n1 = 1.0
    var n1 = 0.0
    for (ord ← 1 to order) {
      val p_n2 = (x*p_n1*(2.0*n1 + 5.0) - p_n0*(n1 + 2.0)) *
                 (n1 + 3.0)/((n1 + 1.0)*(n1 + 5.0))
      p_n0 = p_n1
      p_n1 = p_n2
      n1 += 1.0
    }
    p_n1
  }

  def d1p00(order: Int, x: Double): Double = {
    assert(order >= 0)
    assert((x >= -1.0) && (x <= 1.0))
    val n = order.toDouble
    return 0.5*(n + 1.0)*p11(order-1, x)
  }

  def d1p11(order: Int, x: Double): Double = {
    assert(order >= 0)
    assert((x >= -1.0) && (x <= 1.0))
    val n = order.toDouble
    return 0.5*(n + 3.0)*p22(order-1, x)
  }
}
