package cdmuhlb.dgview

import java.awt.{Color, Dimension, Graphics2D, MouseInfo, Point, Rectangle}
import java.awt.geom.AffineTransform
import java.awt.image.{BufferedImage, AffineTransformOp}
import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import javax.swing.SwingWorker
import scala.concurrent.duration._
import scala.swing.{Component, ProgressBar}
import scala.swing.event.{Key, KeyPressed, MouseEntered, MouseExited, MouseMoved}
import akka.actor.{ActorSystem, Inbox, Props}
import cdmuhlb.dgview.actor.DomainRenderer

class DomainPlot(dom: Domain, pbar: ProgressBar) extends Component {
  private val renderer = new DomainRenderer(this)

  private var worker: renderer.PaintWorker = null
  private var workerSpec: RenderSpec = null

  private var img: BufferedImage = null
  private var lastSpec: RenderSpec = null

  private var imgInProgress: BufferedImage = null
  private var specInProgress: RenderSpec = null

  private var field = dom.elements.head.data.data.keys.head
  def getField: String = field
  def setField(f: String) = {
    require(dom.elements.head.data.data.contains(f))
    if (f != field) {
      field = f
      reset()
    }
  }

  private var colorMap = ContourLinearColorMap(0.0, 5.0, 10,
      DivergingLinearColorMap(0.0, 5.0))
  def getColorMap: ContourLinearColorMap = colorMap
  def setColorMap(cmap: ContourLinearColorMap) {
    require(cmap != null)
    if (cmap != colorMap) {
      colorMap = cmap
      reset()
    }
  }

  private val padding = 8
  private def getMap: PixelMap =
      new PixelMap(PixelBounds(size), DomainBounds(dom), padding)

  private def getSpec: RenderSpec =
      RenderSpec(dom, getField, getColorMap, getMap)
  private def specsMatch(spec: RenderSpec): Boolean = {
    (spec.dom == dom) && (spec.field == field) &&
        (spec.map.scale == getMap.scale) && (spec.colorMap == getColorMap)
  }
  private def validProgress: Boolean =
      (imgInProgress != null) && specsMatch(specInProgress)
  private def validImage: Boolean =
      (img != null) && specsMatch(lastSpec)

  private def reset(): Unit = {
    worker = null
    img = null
    imgInProgress = null
    repaint()
  }

  def progressImage(newImg: BufferedImage, spec: RenderSpec): Unit = {
    if (specsMatch(spec)) {
      imgInProgress = newImg
      specInProgress = spec
      repaint()
    }
  }

  def updateImage(newImg: BufferedImage, spec: RenderSpec): Unit = {
    if (specsMatch(spec)) {
      img = newImg
      lastSpec = spec
      repaint()
    }
  }

  private def startWorker(map: PixelMap): Unit = {
    if ((worker != null) && !specsMatch(workerSpec)) {
      worker.cancel(true)
      worker = null
    }
    if (worker == null) {
      workerSpec = getSpec
      worker = renderer.makeWorker(workerSpec)
      worker.addPropertyChangeListener(
        new PropertyChangeListener {
          def propertyChange(evt: PropertyChangeEvent) {
            (evt.getPropertyName, evt.getNewValue) match {
              case ("progress", x: Integer) ⇒
                pbar.indeterminate = false
                pbar.value = x
              case ("state", SwingWorker.StateValue.STARTED) ⇒
                pbar.indeterminate = true
                pbar.value = 0
              case ("state", SwingWorker.StateValue.DONE) ⇒
                pbar.indeterminate = false
                pbar.value = 0
            }
          }
        }
      )
      worker.execute()
    }
  }


  opaque = true
  minimumSize = new Dimension(2*padding+2, 2*padding+2)
  preferredSize = new Dimension(640, 360)

  override def paintComponent(g: Graphics2D): Unit = {
    // Fill background if opaque
    if (opaque) {
      val dim = size
      g.setColor(background)
      g.fillRect(0, 0, dim.width, dim.height)
    }

    // Render element interiors
    val map = getMap
    if (img == null) {
      startWorker(map)
      if (validProgress) {
        g.drawImage(imgInProgress, null, map.origin.x, map.origin.y)
      }
    } else {
      if (validImage) {
        g.drawImage(img, null, map.origin.x, map.origin.y)
      } else {
        startWorker(map)
        val transOp = new AffineTransformOp(AffineTransform.getScaleInstance(
            lastSpec.map.scale/map.scale, lastSpec.map.scale/map.scale),
            AffineTransformOp.TYPE_NEAREST_NEIGHBOR)
        g.drawImage(img, transOp, map.origin.x, map.origin.y)
        if (validProgress) {
          g.drawImage(imgInProgress, null, map.origin.x, map.origin.y)
        }
      }
    }

    // Draw element borders
    g.setColor(foreground)
    for (elem ← dom.elements) {
      val rect = {
        val xy0 = map.domainToRoundedPixelPoint(
            DomainPoint(elem.xMin, elem.yMax))
        val xy1 = map.domainToRoundedPixelPoint(
            DomainPoint(elem.xMax, elem.yMin))
        new Rectangle(map.origin.x + xy0.x, map.origin.y + xy0.y,
            xy1.x - xy0.x, xy1.y - xy0.y)
      }
      g.draw(rect)
    }
  }
}