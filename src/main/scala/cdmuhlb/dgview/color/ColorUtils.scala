package cdmuhlb.dgview.color

import math.{pow, rint}

object SRgbUtils {
  def forwardTransfer(v: Double): Double = {
    if (v <= 0.0031308) 12.92*v
    else 1.055*pow(v, 1.0/2.4) - 0.055
  }

  def forwardTransfer(v: Float): Float = {
    if (v <= 0.0031308f) 12.92f*v
    else 1.055f*pow(v, 1.0/2.4).toFloat - 0.055f
  }

  def reverseTransfer(v: Double): Double = {
    if (v <= 0.04045) v/12.92
    else pow((v + 0.055)/1.055, 2.4)
  }

  def reverseTransfer(v: Float): Float = {
    if (v <= 0.04045f) v/12.92f
    else pow((v + 0.055f)/1.055f, 2.4).toFloat
  }

  def encode(v: Double): Int = {
    rint(255.0*v).toInt.min(255).max(0)
  }

  def encode(v: Float): Int = {
    rint(255.0f*v).toInt.min(255).max(0)
  }

  def decode(b: Int): Double = {
    require((b >= 0) && (b <= 255))
    b / 255.0
  }

  def decodeFloat(b: Int): Float = {
    require((b >= 0) && (b <= 255))
    b / 255.0f
  }
}

object Bt709Utils {
  def forwardTransfer(c: Double): Double = {
    if (c <= 0.018) 4.5*c
    else 1.099*pow(c, 0.45) - 0.099
  }

  def forwardTransfer(c: Float): Float = {
    if (c <= 0.018f) 4.5f*c
    else 1.099f*pow(c, 0.45).toFloat - 0.099f
  }

  def reverseTransfer(c: Double): Double = {
    if (c <= 0.081) c/4.5
    else pow((c + 0.099)/1.099, 1.0/0.45)
  }

  def reverseTransfer(c: Float): Float = {
    if (c <= 0.081f) c/4.5f
    else pow((c + 0.099f)/1.099f, 1.0/0.45).toFloat
  }

  def eY(r: Double, g: Double, b: Double): Double = {
    0.2126*r + 0.7152*g + 0.0722*b
  }

  def eY(r: Float, g: Float, b: Float): Float = {
    0.2126f*r + 0.7152f*g + 0.0722f*b
  }

  def ePb(b: Double, eY: Double): Double = {
    0.5*(b - eY) / 0.9278
  }

  def ePb(b: Float, eY: Float): Float = {
    0.5f*(b - eY) / 0.9278f
  }

  def ePr(r: Double, eY: Double): Double = {
    0.5*(r - eY) / 0.7874
  }

  def ePr(r: Float, eY: Float): Float = {
    0.5f*(r - eY) / 0.7874f
  }

  def encodeY(eY: Double): Int = {
    rint(219.0*eY + 16.0).toInt.min(254).max(1)
  }

  def encodeY(eY: Float): Int = {
    rint(219.0f*eY + 16.0f).toInt.min(254).max(1)
  }

  def encodeC(eP: Double): Int = {
    rint(224.0*eP + 128.0).toInt.min(254).max(1)
  }

  def encodeC(eP: Float): Int = {
    rint(224.0f*eP + 128.0f).toInt.min(254).max(1)
  }

  def decodeY(d: Int): Double = {
    require((d >= 1) && (d <= 254))
    (d - 16)/219.0
  }
}

object LabUtils {
  // CIE XYZ normalization for 6504 K
  val Xn = 0.95047
  val Yn = 1.0
  val Zn = 1.0883

  def pauliF(q: Double): Double = {
    if (q > 216.0/24389.0) pow(q, 1.0/3.0)
    else (841.0/108.0)*q + 4.0/29.0
  }

  def pauliF(q: Float): Float = {
    if (q > (216.0/24389.0).asInstanceOf[Float]) pow(q, 1.0/3.0).toFloat
    else (841.0/108.0).asInstanceOf[Float]*q + (4.0/29.0).asInstanceOf[Float]
  }

  def pauliFInverse(t: Double): Double = {
    if (t > 6.0/29.0) t*t*t
    else (108.0/841.0)*(t - 4.0/29.0)
  }

  def pauliFInverse(t: Float): Float = {
    if (t > (6.0/29.0).asInstanceOf[Float]) t*t*t
    else (108.0/841.0).asInstanceOf[Float]*(t - (4.0/29.0).asInstanceOf[Float])
  }
}

object ColorUtils {
  /**
    * Lightness is defined to range from 0 to 100.
    * Output ranges from 0 to 1.
    */
  def lightnessToSRgbValue(labL: Double): Double = {
    require((labL >= 0.0) && (labL <= 100.0))
    val t = (labL + 16.0)/116.0
    val y = LabUtils.Yn*LabUtils.pauliFInverse(t)
    SRgbUtils.forwardTransfer(y)
  }
}
