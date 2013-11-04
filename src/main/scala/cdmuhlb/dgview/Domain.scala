package cdmuhlb.dgview

import cdmuhlb.dgview.io.DgElement

case class Domain(elements: Vector[DomainElement]) {
  val xMin = elements.map(_.xMin).min
  val xMax = elements.map(_.xMax).max
  val yMin = elements.map(_.yMin).min
  val yMax = elements.map(_.yMax).max

  def width: Double = xMax - xMin
  def height: Double = yMax - yMin
}

case class DomainElement(xMin: Double, yMin: Double,
    xMax: Double, yMax: Double, data: DgElement) {
  def width: Double = xMax - xMin
  def height: Double = yMax - yMin
}
