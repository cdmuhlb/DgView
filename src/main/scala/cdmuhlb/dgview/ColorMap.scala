package cdmuhlb.dgview

trait ColorMap {
  def map(z: Double): Int
}

case class GammaGrayLinearColorMap(lo: Double, hi: Double) extends ColorMap {
  require(hi > lo)

  def map(z: Double): Int = {
    val v = math.rint(((z - lo)/(hi - lo)).max(0.0).min(1.0)*255.0).toInt
    (0xff<<24) | (v<<16) | (v<<8) | v
  }
}

case class ContourLinearColorMap(lo: Double, hi: Double, nContours: Int,
    map: ColorMap) extends ColorMap {
  require(hi > lo)
  require(nContours > 1)
  val stripeWidth = (hi - lo)/(nContours - 1)

  def map(z: Double): Int = {
    if (z <= lo) map.map(lo)
    else if (z >= hi) map.map(hi)
    else {
      val stripeNum = math.rint((z - lo)/stripeWidth)
      map.map(lo + stripeNum*stripeWidth)
    }
  }
}
