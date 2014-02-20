package cdmuhlb.dgview.color

import scala.math.{acos, atan2, cbrt, cos, pow, sin, sqrt}

case class SRgbColor(r: Double, g: Double, b: Double) {
  def toBytes = ((255.0*r + 0.5).toInt, (255.0*g + 0.5).toInt,
                 (255.0*b + 0.5).toInt)

  def toArgb: Int = {
    import SRgbUtils.encode
    (0xff<<24) | (encode(r)<<16) | (encode(g)<<8) | encode(b)
  }
}

object SRgbColor {
  def fromBytes(r: Int, g: Int, b: Int): SRgbColor =
      SRgbColor(r/255.0, g/255.0, b/255.0)
}

case class CieXyzColor(x: Double, y: Double, z: Double)

case class CieLabColor(l: Double, a: Double, b: Double)

case class MshColor(m: Double, s: Double, h: Double)

object ColorSpaceConversion {
  //private val (xn, yn, zn) = (1.0/3.0, 1.0/3.0, 1.0/3.0)
  //private val (xn, yn, zn) = (1.0, 1.0, 1.0)
  private val (xn, yn, zn) = (0.95047, 1.0, 1.0883)     // 6504 K

  def sRgbToCieXyz(rgb: SRgbColor): CieXyzColor = {
    def linearize(c: Double): Double =
        if (c <= 0.04045) c/12.92
        else pow((c + 0.055)/(1.055), 2.4)
    val rLin = linearize(rgb.r)
    val gLin = linearize(rgb.g)
    val bLin = linearize(rgb.b)
    CieXyzColor(0.4124*rLin + 0.3576*gLin + 0.1805*bLin,
                0.2126*rLin + 0.7152*gLin + 0.0722*bLin,
                0.0193*rLin + 0.1192*gLin + 0.9505*bLin)
  }

  def cieXyzToSRgb(xyz: CieXyzColor): SRgbColor = {
    def gammaCorrect(c: Double): Double =
        if (c <= 0.0031308) 12.92*c
        else 1.055*pow(c, 1.0/2.4) - 0.055
    val rLin =  3.2406*xyz.x - 1.5372*xyz.y - 0.4986*xyz.z
    val gLin = -0.9689*xyz.x + 1.8758*xyz.y + 0.0415*xyz.z
    val bLin =  0.0557*xyz.x - 0.2040*xyz.y + 1.0570*xyz.z
    SRgbColor(gammaCorrect(rLin).max(0.0).min(1.0),
              gammaCorrect(gLin).max(0.0).min(1.0),
              gammaCorrect(bLin).max(0.0).min(1.0))
  }

  def cieXyzToCieLab(xyz: CieXyzColor): CieLabColor = {
    def f(t: Double): Double =
        if (t > 216.0/24389.0) cbrt(t)
        else (841.0/108.0)*t + 4.0/29.0
    CieLabColor(116.0*f(xyz.y/yn) - 16.0,
                500.0*(f(xyz.x/xn) - f(xyz.y/yn)),
                200.0*(f(xyz.y/yn) - f(xyz.z/zn)))
  }

  def cieLabToCieXyz(lab: CieLabColor): CieXyzColor = {
    def finv(t: Double): Double =
        if (t > 6.0/29.0) t*t*t
        else (108.0/841.0)*(t - 4.0/29.0)
    val c = (lab.l + 16.0)/116.0
    CieXyzColor(xn*finv(c + lab.a/500.0),
                yn*finv(c),
                zn*finv(c - lab.b/200.0))
  }

  def cieLabToMsh(lab: CieLabColor): MshColor = {
    val m = sqrt(lab.l*lab.l + lab.a*lab.a + lab.b*lab.b)
    MshColor(m, acos(lab.l/m), atan2(lab.b, lab.a))
  }

  def mshToCieLab(msh: MshColor): CieLabColor =
      CieLabColor(msh.m*cos(msh.s), msh.m*sin(msh.s)*cos(msh.h),
                  msh.m*sin(msh.s)*sin(msh.h))

  def mshToSRgb(msh: MshColor): SRgbColor =
      cieXyzToSRgb(cieLabToCieXyz(mshToCieLab(msh)))

  def sRgbToMsh(rgb: SRgbColor): MshColor =
      cieLabToMsh(cieXyzToCieLab(sRgbToCieXyz(rgb)))
}
