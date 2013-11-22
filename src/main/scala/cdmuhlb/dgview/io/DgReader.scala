package cdmuhlb.dgview.io

import java.io.File
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.SortedMap
import scala.io.Source
import cdmuhlb.dgview.spectral.LegendreGaussLobatto

case class Coord2D(x: Double, y: Double)

case class DataPoint2D(coords: Coord2D, data: Map[Int, Double])

case class DgElement(nx: Int, ny: Int, coords: Vector[Coord2D],
    data: Map[String, Vector[Vector[Double]]]) {
  assert((nx > 0) && (ny > 0))
  assert(coords.length == nx*ny)
  for (v ← data.values) {
    assert(v.length == ny)
    for (row ← v) assert(row.length == nx)
  }
  val xBasis = LegendreGaussLobatto(nx)
  val yBasis = LegendreGaussLobatto(ny)

  def interpolateTo(field: String, x: Double, y: Double): Double = {
    // Clamp to reference element
    val topoX = (2.0*(x - xMin)/(xMax - xMin) - 1.0).max(-1.0).min(1.0)
    val topoY = (2.0*(y - yMin)/(yMax - yMin) - 1.0).max(-1.0).min(1.0)

    // Cardinal function interpolation appears to be faster than Fornberg
    // interpolation for single points.
    data(field).map(_.zipWithIndex.map{case (z, i) ⇒
        z*xBasis.cardinal(i, topoX)}.sum).zipWithIndex.map{case (z, j) ⇒
        z*yBasis.cardinal(j, topoY)}.sum
  }

  def interpolateToGrid(field: String, xs: Vector[Double],
      ys: Vector[Double]): Vector[Vector[Double]] = {
    // Clamp to reference element
    val topoXs = xs.map(x ⇒
        (2.0*(x - xMin)/(xMax - xMin) - 1.0).max(-1.0).min(1.0))
    val topoYs = ys.map(y ⇒
        (2.0*(y - yMin)/(yMax - yMin) - 1.0).max(-1.0).min(1.0))

    val xMat = xBasis.interpolationMatrix(topoXs)
    val yMat = yBasis.interpolationMatrix(topoYs)

    def transpose(mat: Vector[Vector[Double]]): Vector[Vector[Double]] = {
      (for (i ← 0 until mat.head.length) yield {
        (for (j ← 0 until mat.length) yield mat(j)(i)).toVector
      }).toVector
    }
    def ddot(v1: Vector[Double], v2: Vector[Double]): Double = {
      assert(v1.length == v2.length)
      v1.zip(v2).map{case (a, b) ⇒ a*b}.sum
    }
    def dgemv(mat: Vector[Vector[Double]],
        vec: Vector[Double]): Vector[Double] = {
      assert(mat.length > 0)
      assert(mat.head.length == vec.length)
      mat.map(row ⇒ ddot(row, vec))
    }
    def dgemm(m1: Vector[Vector[Double]],
        m2: Vector[Vector[Double]]): Vector[Vector[Double]] = {
      assert(m1.length > 0)
      assert(m1.head.length == m2.length)
      val m2t = transpose(m2)
      m1.map(r ⇒ m2t.map(c ⇒ ddot(r, c)))
    }

    val ans = dgemm(yMat, data(field).map(dgemv(xMat, _)))
    assert((ans.length == ys.length) && (ans.head.length == xs.length))
    ans
  }

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
          ArrayBuffer[Vector[Double]]]
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
                ArrayBuffer.empty[Vector[Double]]
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
