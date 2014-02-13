package cdmuhlb.dgview.io

import java.io.File
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable.SortedMap
import scala.io.Source
import breeze.linalg.{DenseMatrix, DenseVector}
import cdmuhlb.dgview.spectral.LegendreGaussLobatto

case class Coord2D(x: Double, y: Double)

case class DataPoint2D(coords: Coord2D, data: Map[Int, Double])

object DgElement {
  private val lglCache = mutable.HashMap.empty[Int, LegendreGaussLobatto]

  def lglElement(nNodes: Int): LegendreGaussLobatto =
      lglCache.getOrElseUpdate(nNodes, LegendreGaussLobatto(nNodes))
}

case class DgElement(nx: Int, ny: Int, coords: Vector[Coord2D],
    data: Map[String, Vector[Vector[Double]]]) {
  // Check preconditions
  assert((nx > 0) && (ny > 0))
  assert(coords.length == nx*ny)
  for (v ← data.values) {
    assert(v.length == ny)
    for (row ← v) assert(row.length == nx)
  }

  // Construct reference element
  val xBasis = DgElement.lglElement(nx)
  val yBasis = DgElement.lglElement(ny)

  // Copy data to high-performance matrix class
  val dataMatrix = data.map{case (field, values) ⇒ (field → {
    val mat = DenseMatrix.zeros[Double](ny, nx)
    for (i ← 0 until nx; j ← 0 until ny) mat(j, i) = values(j)(i)
    mat
  })}

  def interpolateTo(field: String, x: Double, y: Double): Double = {
    // Map and clamp to reference element
    val topoX = (2.0*(x - xMin)/(xMax - xMin) - 1.0).max(-1.0).min(1.0)
    val topoY = (2.0*(y - yMin)/(yMax - yMin) - 1.0).max(-1.0).min(1.0)

    // Perform cardinal function interpolation
    data(field).map(_.zipWithIndex.map{case (z, i) ⇒
        z*xBasis.cardinal(i, topoX)}.sum).zipWithIndex.map{case (z, j) ⇒
        z*yBasis.cardinal(j, topoY)}.sum
  }

  def interpolateToGrid(field: String, xs: Vector[Double],
      ys: Vector[Double]): Vector[Vector[Double]] = {
    // Map and clamp to reference element
    val topoXs = xs.map(x ⇒
        (2.0*(x - xMin)/(xMax - xMin) - 1.0).max(-1.0).min(1.0))
    val topoYs = ys.map(y ⇒
        (2.0*(y - yMin)/(yMax - yMin) - 1.0).max(-1.0).min(1.0))

    // Create interpolation matrices for x and y coordinates
    val xMat = xBasis.interpolationMatrix(topoXs)
    val yMat = yBasis.interpolationMatrix(topoYs)

    // Interpolate via matrix multiplication
    val zMat = yMat*(xMat*dataMatrix(field).t).t

    // Convert to nested Scala vectors
    (for (i ← 0 until zMat.rows) yield
        zMat(i, ::).t(::, 0).toArray.toVector).toVector
  }

  def dataMin(field: String): Double = data(field).map(_.min).min
  def dataMax(field: String): Double = data(field).map(_.max).max

  val xMin: Double = coords.map(_.x).min
  val yMin: Double = coords.map(_.y).min
  val xMax: Double = coords.map(_.x).max
  val yMax: Double = coords.map(_.y).max
}

class FhebertDataDir(dir: File) {
  val pat = """VarsTime(\d+).data""".r
  def domains(): SortedMap[Int, FhebertDataFile] = {
    require(dir.isDirectory)
    SortedMap(dir.list.collect{case filename @ pat(time) ⇒
      (time.toInt → new FhebertDataFile(new File(dir, filename)))}:_*)
  }
}

class FhebertDataFile(file: File) {
  private val pat = """# \[(\d+)\] (.*)""".r

  def elements(): Vector[DgElement] = {
    @tailrec
    def parseHeader(lines: List[String], ans: Map[Int, String]):
        (List[String], Map[Int, String]) = lines match {
      case pat(colNum, fieldName) :: tail ⇒
        parseHeader(tail, ans + (colNum.toInt → fieldName))
      case _ ⇒ (lines, ans)
    }

    def parseLine(line: String): DataPoint2D = {
      val fields = line.split("\\s+")
      val x = fields(0).toDouble
      val y = fields(1).toDouble
      var data = Map.empty[Int, Double]
      for (i ← 2 until fields.length) {
        data += ((i+1) → fields(i).toDouble)
      }
      DataPoint2D(Coord2D(x, y), data)
    }
    
    @tailrec
    def parseRow(lines: List[String], ans: Vector[DataPoint2D]): (
        List[String], Vector[DataPoint2D]) = lines match {
      case head :: tail ⇒
        if (head.nonEmpty) parseRow(tail, ans :+ parseLine(head.trim))
        else (tail, ans)
      case Nil ⇒ (Nil, ans)
    }
    
    def parseElement(lines: List[String], header: Map[Int, String]):
        (List[String], DgElement) = {
      var coords = Vector.empty[Coord2D]
      var data = scala.collection.mutable.Map.empty[String,
          mutable.ArrayBuffer[Vector[Double]]]
      var myLines = lines
      var row = {
        val (tmpLines, tmpRow) = parseRow(myLines, Vector.empty[DataPoint2D])
        myLines = tmpLines
        tmpRow
      }
      assert(row.nonEmpty)
      var ny = 0
      val nx = row.length
      while (row.nonEmpty) {
        assert(row.length == nx)
        ny += 1
        for (dp ← row) {
          coords :+= dp.coords
          for ((k, v) ← dp.data) {
            val ks = header(k)
            if (!data.isDefinedAt(ks)) data(ks) =
                mutable.ArrayBuffer.empty[Vector[Double]]
            if (data(ks).length < ny) data(ks) += Vector.empty[Double]
            data(ks)(ny-1) :+= v
          }
        }
        row = {
          val (tmpLines, tmpRow) =
              parseRow(myLines, Vector.empty[DataPoint2D])
          myLines = tmpLines
          tmpRow
        }
      }
      // TODO: Reverse data if required
      (myLines, DgElement(nx, ny, coords, data.mapValues(_.toVector).toMap))
    }
    
    val src = Source.fromFile(file)
    var lines = src.getLines.toList
    val header = {
      val (tmpLines, tmpHeader) = parseHeader(lines, Map.empty[Int, String])
      lines = tmpLines
      tmpHeader
    }
    var ans = Vector.empty[DgElement]
    while (lines.nonEmpty) {
      ans :+= {
        val (tmpLines, tmpElem) = parseElement(lines, header)
        lines = tmpLines
        tmpElem
      }
    }
    src.close()
    ans
  }
}
