package cdmuhlb.dgview.io

import java.io.{BufferedReader, File, FileReader, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import cdmuhlb.dgview.spectral.LegendreGaussLobatto

case class Coord2D(x: Double, y: Double)

case class DataPoint2D(coords: Coord2D, data: Map[String, Double])

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

    data(field).map(_.zipWithIndex.map{case (z, i) ⇒
        z*xBasis.cardinal(i, topoX)}.sum).zipWithIndex.map{case (z, j) ⇒
        z*yBasis.cardinal(j, topoY)}.sum
  }

  val xMin: Double = coords.map(_.x).min
  val yMin: Double = coords.map(_.y).min
  val xMax: Double = coords.map(_.x).max
  val yMax: Double = coords.map(_.y).max
}

class FhebertDataFile(file: File) {
  def elements(): Vector[DgElement] = {
    val in = new BufferedReader(new FileReader(file))

    def nextLine(): String = {
      val line = in.readLine()
      if (line == null) ""
      else if (line.nonEmpty && (line.head == '#')) nextLine()
      else line.trim
    }

    def parseLine(line: String): DataPoint2D = {
      val fields = line.split("\\s+")
      val x = fields(2).toDouble
      val y = fields(3).toDouble
      var data = Map.empty[String, Double]
      for (i ← 4 until fields.length) {
        data += (s"Field_${i+1}" → fields(i).toDouble)
      }
      DataPoint2D(Coord2D(x, y), data.toMap)
    }

    def readRow(): Vector[DataPoint2D] = {
      var ans = Vector.empty[DataPoint2D]
      var line = nextLine()
      while (line.nonEmpty) {
        ans :+= parseLine(line)
        line = nextLine()
      }
      ans
    }

    def readElement(): Option[DgElement] = {
      var coords = Vector.empty[Coord2D]
      var data = scala.collection.mutable.Map.empty[String,
          ArrayBuffer[Vector[Double]]]
      var row = readRow()
      if (row.isEmpty) None
      else {
        var ny = 0
        val nx = row.length
        while (row.nonEmpty) {
          assert(row.length == nx)
          ny += 1
          for (dp ← row) {
            coords :+= dp.coords
            for ((k, v) ← dp.data) {
              if (!data.isDefinedAt(k)) data(k) =
                  ArrayBuffer.empty[Vector[Double]]
              if (data(k).length < ny) data(k) += Vector.empty[Double]
              data(k)(ny-1) :+= v
            }
          }
          row = readRow()
        }
        Some(DgElement(nx, ny, coords, data.mapValues(_.toVector).toMap))
      }
    }

    var ans = Vector.empty[DgElement]
    var elem = readElement()
    while (elem.nonEmpty) {
      ans :+= elem.get
      elem = readElement()
    }

    in.close()
    ans
  }
}
