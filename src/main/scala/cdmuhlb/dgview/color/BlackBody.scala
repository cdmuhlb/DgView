package cdmuhlb.dgview.color

import scala.math.expm1

object BlackBody {
  val PlanckConstant = 6.62606957e-34
  val SpeedOfLight = 299792458.0
  val BoltzmannConstant = 1.3806488e-23

  def pow5(x: Double): Double = {
    val x2 = x*x
    x2*x2*x
  }
}

case class BlackBody(temp: Double) {
  import BlackBody._
  val hckt = SpeedOfLight*PlanckConstant / (BoltzmannConstant*temp)
  def spectralRadiance(lambda: Double): Double = {
    2.0*PlanckConstant*SpeedOfLight*SpeedOfLight /
        (expm1(hckt/lambda)*pow5(lambda))
  }
}

