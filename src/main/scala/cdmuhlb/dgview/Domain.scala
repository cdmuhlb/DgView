package cdmuhlb.dgview

import scala.collection.immutable.{SortedMap, SortedSet}
import cdmuhlb.dgview.io.{DgElement, FhebertDataDir}

object DomainSeq {
  def apply(dir: FhebertDataDir): DomainSeq = {
    DomainSeq(dir.domains.map{case (t, f) ⇒
        (t → Domain(f.elements.map(dge ⇒
        DomainElement(dge.xMin, dge.yMin, dge.xMax, dge.yMax, dge))))})
  }
}

case class DomainSeq(domains: SortedMap[Int, Domain]) {
  def times: SortedSet[Int] = domains.keySet
  def fields: List[String] = {
    if (domains.isEmpty) List.empty[String]
    else domains.values.head.fields
  }
}

case class Domain(elements: Vector[DomainElement]) {
  val xMin = elements.map(_.xMin).min
  val xMax = elements.map(_.xMax).max
  val yMin = elements.map(_.yMin).min
  val yMax = elements.map(_.yMax).max

  def width: Double = xMax - xMin
  def height: Double = yMax - yMin
  def fields: List[String] = {
    if (elements.nonEmpty) elements.head.data.data.keys.toList.sorted
    else List.empty[String]
  }
}

case class DomainElement(xMin: Double, yMin: Double,
    xMax: Double, yMax: Double, data: DgElement) {
  def width: Double = xMax - xMin
  def height: Double = yMax - yMin
}
