package cdmuhlb.dgview

import scala.math.{acos, atan2, cbrt, cos, pow, sin, sqrt}
import cdmuhlb.dgview.color.{ColorSpaceConversion, SRgbColor, CieXyzColor, MshColor, ColorUtils}

trait ColorMap {
  def map(z: Double): Int
  def getFactory: ColorMapFactory
}

trait ColorMapFactory {
  def createMap(lo: Double, hi: Double): ColorMap
}

object GammaGrayLinearFactory extends ColorMapFactory {
  def createMap(lo: Double, hi: Double) = GammaGrayLinearColorMap(lo, hi)
  override def toString(): String = "sRGB gray"
}

case class GammaGrayLinearColorMap(lo: Double, hi: Double) extends ColorMap {
  require(hi > lo)

  def map(z: Double): Int = {
    val v = math.rint(((z - lo)/(hi - lo)).max(0.0).min(1.0)*255.0).toInt
    (0xff<<24) | (v<<16) | (v<<8) | v
  }

  def getFactory = GammaGrayLinearFactory
}

object LabGrayFactory extends ColorMapFactory {
  def createMap(lo: Double, hi: Double) = LabGrayColorMap(lo, hi)
  override def toString(): String = "Lab gray"
}

case class LabGrayColorMap(lo: Double, hi: Double) extends ColorMap {
  require(hi > lo)

  def map(z: Double): Int = {
    val zNorm = ((z - lo)/(hi - lo)).max(0.0).min(1.0)
    val v = ColorUtils.lightnessToSRgbValue(100.0*zNorm)
    (0xff<<24) | (v<<16) | (v<<8) | v
  }

  def getFactory = LabGrayFactory
}

object BlackbodyFactory extends ColorMapFactory {
  def createMap(lo: Double, hi: Double) = BlackbodyColorMap(lo, hi)
  override def toString(): String = "Blackbody"
}

case class BlackbodyColorMap(lo: Double, hi: Double) extends ColorMap {
  require(hi > lo)

  def map(z: Double): Int = {
    val tMax = 6500.0
    val tMin = 1667.0
    val t = ((z - lo)/(hi - lo)).max(0.0).min(1.0)*(tMax - tMin) + tMin
    val invT = 1.0 / t
    val invT2 = invT * invT
    val invT3 = invT2 * invT
    val x = if (t < 4000.0) {
      -0.2661239e9*invT3 - 0.2343580e6*invT2 + 0.8776956e3*invT + 0.179910
    } else {
      -3.0258469e9*invT3 + 2.1070379e6*invT2 + 0.2226347e3*invT + 0.240390
    }
    val x2 = x * x
    val x3 = x2 * x
    val y = if (t < 2222.0) {
      -1.1063814*x3 - 1.34811020*x2 + 2.18555832*x - 0.20219683
    } else if (t < 4000.0f) {
      -0.9549476*x3 - 1.37418593*x2 + 2.09137015*x - 0.16748867
    } else {
      3.0817580*x3 - 5.87338670*x2 + 3.75112997*x - 0.37001483
    }

    val xyzY = math.pow(t/tMax, 4).max(0.0).min(1.0)
    val xyzX = (xyzY * x / y).max(0.0).min(1.0)
    val xyzZ = (xyzY * (1.0 - x - y) / y).max(0.0).min(1.0)
    val (cr, cg, cb) = ColorSpaceConversion.cieXyzToSRgb(
        CieXyzColor(xyzX, xyzY, xyzZ)).toBytes
    (0xff<<24) | (cr<<16) | (cg<<8) | cb
  }

  def getFactory = BlackbodyFactory
}

case class ContourLinearFactory(nContours: Int, map: ColorMap)
    extends ColorMapFactory {
  def createMap(lo: Double, hi: Double) =
      ContourLinearColorMap(lo, hi, nContours, map)
  override def toString(): String = "sContour($map)"
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

  lazy val getFactory = ContourLinearFactory(nContours, map)

  def withColormap(newMap: ColorMap): ContourLinearColorMap =
      ContourLinearColorMap(lo, hi, nContours, newMap)

  def withFactory(factory: ColorMapFactory): ContourLinearColorMap =
      withColormap(factory.createMap(lo, hi))

  def withContours(newContours: Int) =
      ContourLinearColorMap(lo, hi, newContours, map)
}

object DivergingLinearFactory extends ColorMapFactory {
  def createMap(lo: Double, hi: Double) = DivergingLinearColorMap(lo, hi)
  override def toString(): String = "Diverging"
}

case class DivergingLinearColorMap(lo: Double, hi: Double) extends ColorMap {
  val mshFunc = ColorSpaceInterpolation.mshGradient4(
      MshColor(80.0, 1.08, -1.1), MshColor(88.0, 0.0, -1.661),
      MshColor(88.0, 0.0, 1.061), MshColor(80.0, 1.08, 0.5))_

  def map(z: Double): Int = {
    val zNorm = (2.0*(z - lo)/(hi - lo) - 1.0).max(-1.0).min(1.0)
    val (cr, cg, cb) = ColorSpaceConversion.mshToSRgb(mshFunc(zNorm)).toBytes
    (0xff<<24) | (cr<<16) | (cg<<8) | cb
  }

  def getFactory = DivergingLinearFactory
}

object MshRainbowColorMapFactory extends ColorMapFactory {
  def createMap(lo: Double, hi: Double) = MshRainbowColorMap(lo, hi)
  override def toString(): String = "Msh rainbow"
}

/**
  * Spiral in saturation and hue achieving perceptual uniformity.
  * The parameters chosen here ensure that all colors remain in the sRGB gamut.
  *
  * To design custom MshRainbow colormaps, see
  * https://github.com/cdmuhlb/MshExplorer .
  */
case class MshRainbowColorMap(lo: Double, hi: Double) extends ColorMap {
  val (m, s0, sf, h0, hRate) = (90.5, 1.25, 0.0, -1.03, -3.52)
  // These alternate parameters define a map that is more uniform in lightness
  //val (m, s0, sf, h0, hRate) = (95.0, 1.2, 0.475, -1.1, -3.33)

  def map(z: Double): Int = {
    val zNorm01 = ((z - lo)/(hi - lo)).max(0.0).min(1.0)
    val s = (sf - s0)*zNorm01 + s0
    val h = if (s <= 0.0) 0.0 else if (s >= 0.5*math.Pi) h0 else {
      (h0 + hRate*math.log(math.tan(0.5*s0))/(sf - s0)) -
          hRate*math.log(math.tan(0.5*s))/(sf - s0)
    }
    val msh = MshColor(m, s, h)
    val (cr, cg, cb) = ColorSpaceConversion.mshToSRgb(msh).toBytes
    (0xff<<24) | (cr<<16) | (cg<<8) | cb
  }
  def getFactory = MshRainbowColorMapFactory
}


object ColorSpaceInterpolation {
  def mshGradient(c1: MshColor, c2: MshColor, x: Double): MshColor = {
    //assert(c1.m == c2.m)
    val m = 0.5*(c1.m + c2.m)
    val s = if (x < 0) -x*c1.s else x*c2.s
    val h = if (x < 0) c1.h else c2.h
    MshColor(m, s, h)
  }

  def mshGradient4(cL: MshColor, cML: MshColor, cMR: MshColor, cR: MshColor)
                  (x: Double): MshColor = {
    val (a, b, c1, c2) = if (x < 0) (-x,      1.0 + x, cL,  cML)
                         else       (1.0 - x, x,       cMR, cR )
    MshColor(a*c1.m + b*c2.m, a*c1.s + b*c2.s, a*c1.h + b*c2.h)
  }
}
