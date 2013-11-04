package cdmuhlb.dgview

import java.awt.Dimension

case class PixelBounds(width: Int, height: Int)
object PixelBounds {
  def apply(dim: Dimension): PixelBounds = PixelBounds(dim.width, dim.height)
}

case class DomainBounds(xMin: Double, yMin: Double,
    width: Double, height: Double)
object DomainBounds {
  def apply(dom: Domain): DomainBounds = DomainBounds(dom.xMin, dom.yMin,
      dom.width, dom.height)
  def apply(elem: DomainElement): DomainBounds = DomainBounds(elem.xMin,
      elem.yMin, elem.width, elem.height)
}

case class PixelPoint(x: Int, y: Int)

case class DomainPoint(x: Double, y: Double)

case class PixelMap(pb: PixelBounds, db: DomainBounds, padding: Int) {
  val scale = {
    val maxWidth = pb.width - 2*padding
    val maxHeight = pb.height - 2*padding
    require((maxWidth > 1) && (maxHeight > 1))
    (db.width/(maxWidth - 1)).max(db.height/(maxHeight - 1))
  }
  val bounds = {
    val width = (db.width/scale + 1).toInt
    val height = (db.height/scale + 1).toInt
    PixelBounds(width, height)
  }
  val origin = {
    val x0 = (pb.width - bounds.width)/2
    val y0 = (pb.height - bounds.height)/2
    PixelPoint(x0, y0)
  }

  def domainToPixelX(x: Double): Double = (x - db.xMin)/scale
  def domainToPixelY(y: Double): Double = bounds.height - 1 -
      (y - db.yMin)/scale

  def pixelToDomainX(x: Int): Double = db.xMin + x*scale
  def pixelToDomainY(y: Int): Double = db.yMin +
      (bounds.height - 1 - y)*scale

  def lowerPixelPoint(dom: DomainBounds): PixelPoint = {
    val x = math.ceil(domainToPixelX(dom.xMin)).toInt
    val y = math.ceil(domainToPixelY(dom.yMin + dom.height)).toInt
    PixelPoint(x, y)
  }

  def upperPixelPoint(dom: DomainBounds): PixelPoint = {
    val x = (domainToPixelX(dom.xMin + dom.width)).toInt
    val y = (domainToPixelY(dom.yMin)).toInt
    PixelPoint(x, y)
  }

  def domainToRoundedPixelPoint(p: DomainPoint): PixelPoint = {
    val x = math.rint(domainToPixelX(p.x)).toInt
    val y = math.rint(domainToPixelY(p.y)).toInt
    PixelPoint(x, y)
  }

  def pixelToDomainPoint(p: PixelPoint) = DomainPoint(pixelToDomainX(p.x),
      pixelToDomainY(p.y))
}
