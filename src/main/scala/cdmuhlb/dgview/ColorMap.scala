package cdmuhlb.dgview

import cdmuhlb.dgview.color.{ColorSpaceConversion, SRgbColor, CieXyzColor, MshColor}
import cdmuhlb.dgview.color.{ColorUtils, SRgbUtils}

trait ColorMap {
  def mapToArgb(z: Double): Int
}

trait NormalizedColorMap {
  def mapToArgb(zNorm: Double): Int
  def name: String

  // For use by GUI controls
  override def toString(): String = name

  def mkGnuplotPalette(nEntries: Int): String = {
    val entries = for (i ← 0 until nEntries) yield {
      val z = i.toDouble/(nEntries - 1)
      val c = mapToArgb(z) & 0xffffff
      f"$z%.6f '#$c%06x'"
    }
    entries.mkString("set palette defined(", ", ", ")")
  }
}

object SRgbGrayMap extends NormalizedColorMap {
  def mapToArgb(zNorm: Double): Int = {
    val v = SRgbUtils.encode(zNorm)
    (0xff<<24) | (v<<16) | (v<<8) | v
  }
  val name = "sRgb gray"
}

object LabGrayMap extends NormalizedColorMap {
  def mapToArgb(zNorm: Double): Int = {
    val v = ColorUtils.lightnessToSRgbValue(100.0*zNorm)
    (0xff<<24) | (v<<16) | (v<<8) | v
  }
  val name = "Lab gray"
}

// This map is not currently functioning as desired
object BlackbodyMap extends NormalizedColorMap {
  def mapToArgb(zNorm: Double): Int = {
    val tMax = 6500.0
    val tMin = 1667.0
    val t = zNorm*(tMax - tMin) + tMin
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
    ColorSpaceConversion.cieXyzToSRgb(CieXyzColor(xyzX, xyzY, xyzZ)).toArgb
  }
  val name = "Blackbody"
}

case class DivergingMap(cL: MshColor, cML: MshColor, cMR: MshColor,
    cR: MshColor, id: String) extends NormalizedColorMap {
  def mapToArgb(zNorm: Double): Int = {
    val zz = 2.0*zNorm - 1.0
    val (a, b, c1, c2) = if (zz < 0) (-zz,      1.0 + zz, cL,  cML)
                         else        (1.0 - zz, zz,       cMR, cR )
    val msh = MshColor(a*c1.m + b*c2.m, a*c1.s + b*c2.s, a*c1.h + b*c2.h)
    ColorSpaceConversion.mshToSRgb(msh).toArgb
  }
  def name = "Diverging " + id
}

object DivergingMap {
  def fromEndpoints(cL: SRgbColor, cR: SRgbColor, id: String): DivergingMap = {
    import ColorSpaceConversion.sRgbToMsh
    fromEndpoints(sRgbToMsh(cL), sRgbToMsh(cR), id)
  }

  def fromEndpoints(cL: MshColor, cR: MshColor, id: String): DivergingMap = {
    import ColorSpaceInterpolation.adjustHue
    val mM = cL.m.max(cR.m.max(88.0))
    val hML = adjustHue(cL, mM)
    val hMR = adjustHue(cR, mM)
    DivergingMap(cL, MshColor(mM, 0.0, hML), MshColor(mM, 0.0, hMR), cR, id)
  }

  // Cool to warm (Paraview default)
  val preset1 = fromEndpoints(SRgbColor(0.230, 0.299, 0.754),
      SRgbColor(0.706, 0.016, 0.150), "cool/warm")

  // Purple to orange
  val preset2 = fromEndpoints(SRgbColor(0.436, 0.308, 0.631),
      SRgbColor(0.759, 0.334, 0.046), "purple/orange")

  // Green to purple
  val preset3 = fromEndpoints(SRgbColor(0.085, 0.532, 0.201),
      SRgbColor(0.436, 0.308, 0.631), "green/purple")

  // Blue to tan
  val preset4 = fromEndpoints(SRgbColor(0.217, 0.525, 0.910),
      SRgbColor(0.677, 0.492, 0.093), "blue/tan")

  // Green to red
  val preset5 = fromEndpoints(SRgbColor(0.085, 0.532, 0.201),
      SRgbColor(0.758, 0.214, 0.233), "green/red")
}

/**
  * Spiral in saturation and hue achieving perceptual uniformity.
  *
  * To design custom MshRainbow colormaps, see
  * https://github.com/cdmuhlb/MshExplorer .
  */
case class MshRainbowMap(m: Double, s0: Double, sf: Double,
    h0: Double, hRate: Double, id: String) extends NormalizedColorMap {
  import math.{Pi, log, tan}
  private val hOffset = h0 + hRate*log(tan(0.5*s0))/(sf - s0)

  def mapToArgb(zNorm: Double): Int = {
    val s = (sf - s0)*zNorm + s0
    val h = if (s <= 0.0) 0.0 else if (s >= 0.5*Pi) h0 else
        hOffset - hRate*log(tan(0.5*s))/(sf - s0)
    ColorSpaceConversion.mshToSRgb(MshColor(m, s, h)).toArgb
  }
  def name = "Msh rainbow " + id
}

object MshRainbowMap {
  // The parameters chosen for these presets ensure that all colors remain in
  //   the sRGB gamut

  // Spiral from deep purple through red to light green
  val preset1 = MshRainbowMap(90.5, 1.25, 0.0, -1.03, -3.52, "1")

  // More uniform in lightness, so may be better for shading
  val preset1b = MshRainbowMap(95.0, 1.2, 0.475, -1.1, -3.33, "1b")

  // Spiral from deep red through blue to medium green
  val preset2 = MshRainbowMap(84.25, 1.1, 0.215, 0.725, 3.0, "2")
}

case class ContourLinearColorMap(lo: Double, hi: Double, nContours: Int,
    map: NormalizedColorMap) extends ColorMap {
  require(hi > lo)
  require(nContours > 1)
  val stripeWidth = (hi - lo)/(nContours - 1)

  def mapToArgb(z: Double): Int = {
    if (z <= lo) map.mapToArgb(0.0)
    else if (z >= hi) map.mapToArgb(1.0)
    else {
      val stripeNum = math.rint((z - lo)/stripeWidth)
      val zz = lo + stripeNum*stripeWidth
      val zNorm = ((zz - lo)/(hi - lo)).min(1.0).max(0.0)
      map.mapToArgb(zNorm)
    }
  }

  def withColormap(newMap: NormalizedColorMap): ContourLinearColorMap =
      ContourLinearColorMap(lo, hi, nContours, newMap)

  def withContours(newContours: Int) =
      ContourLinearColorMap(lo, hi, newContours, map)

  // Note: Resulting palette "gray" range is [0, 1], not [lo, hi]
  def mkGnuplotPalette: String = {
    def mkEntry(z: Double, c: Int) = f"$z%.6f '#$c%06x'"

    val interiorEntries = for (i ← 1 until (nContours - 1)) yield {
      val zLo = (i - 0.5)/(nContours - 1)
      val z = i.toDouble/(nContours - 1)
      val zHi = (i + 0.5)/(nContours - 1)
      val c = mapToArgb(z) & 0xffffff
      mkEntry(zLo, c) + ", " + mkEntry(zHi, c)
    }
    val firstEntry = {
      val c = mapToArgb(0.0) & 0xffffff
      mkEntry(0.0, c) + ", " + mkEntry(0.5/(nContours - 1), c)
    }
    val lastEntry = {
      val c = mapToArgb(1.0) & 0xffffff
      mkEntry(1.0 - 0.5/(nContours - 1), c) + ", " + mkEntry(1.0, c)
    }
    interiorEntries.mkString(s"set palette defined($firstEntry, ", ", ",
        s", $lastEntry)")
  }
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

  def adjustHue(c: MshColor, m: Double): Double = {
    import math.{Pi, sin, sqrt}
    if (c.m >= m) c.h
    else {
      val hSpin = c.s*sqrt(m*m - c.m*c.m) / (c.m*sin(c.s))
      if (c.h > -Pi/3.0) c.h + hSpin else c.h - hSpin
    }
  }
}
