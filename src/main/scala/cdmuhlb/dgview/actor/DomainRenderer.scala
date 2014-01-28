package cdmuhlb.dgview.actor

import java.awt.{Color, Dimension, Rectangle}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import javax.swing.SwingWorker
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import akka.actor.{ActorSystem, Inbox, Props}
import cdmuhlb.dgview.{RenderingRecipient, DomainPoint, PixelPoint, RenderSpec}

object RenderSystem {
  val system = ActorSystem("RenderSystem")
  val actor = system.actorOf(Props[RenderActor].withDispatcher(
    "render-dispatcher"))
  for (i ← 1 until Runtime.getRuntime().availableProcessors) {
    system.actorOf(Props[RenderActor].withDispatcher("render-dispatcher"))
  }
}

class DomainRenderer(plot: RenderingRecipient) {
  private var counter = 0

  def makeWorker(spec: RenderSpec): PaintWorker = {
    counter += 1
    new PaintWorker(counter, spec)
  }

  class PaintWorker(seqNum: Int, spec: RenderSpec)
      extends SwingWorker[BufferedImage, (PixelPoint, BufferedImage)] {
    val elements = spec.dom.elements

    override def doInBackground(): BufferedImage = {
      val inbox = Inbox.create(RenderSystem.system)
      for (elem ← elements) {
        inbox.send(RenderSystem.actor,
            SequencedMessage(seqNum, RenderActor.Render(spec, elem)))
      }

      val domImg = new BufferedImage(spec.map.bounds.width,
          spec.map.bounds.height, BufferedImage.TYPE_INT_ARGB)
      val g = domImg.createGraphics

      val nRequests = elements.length
      var nResponses = 0
      while ((nResponses < nRequests) && !isCancelled) {
        inbox.receive(1.minute) match {
          case RenderActor.Rendering(s, elem, origin, imgOpt) ⇒
            assert(s == seqNum)
            for (img ← imgOpt) {
              g.drawImage(img, null, origin.x, origin.y)
              publish((origin, img))
            }
            nResponses += 1
            setProgress(100*nResponses/nRequests)
        }
      }
      domImg
    }

    override protected def process(chunks:
        java.util.List[(PixelPoint, BufferedImage)]): Unit = {
      if (!isCancelled) {
        plot.progressImages(chunks.asScala, spec)
      }
    }

    override protected def done(): Unit = {
      if (!isCancelled) {
        plot.updateImage(get, spec)
      }
    }
  }
}

class AnimationWorker(specs: Seq[RenderSpec], dir: File)
    extends SwingWorker[Unit, Unit] {
  override def doInBackground(): Unit = {
    val inbox = Inbox.create(RenderSystem.system)
    val nSpecs = specs.length
    for ((spec, step) ← specs.zipWithIndex) {
      val elements = spec.dom.elements
      for (elem ← elements) {
        inbox.send(RenderSystem.actor,
            RenderActor.Render(spec, elem))
      }

      val domImg = new BufferedImage(spec.map.pb.width,
          spec.map.pb.height, BufferedImage.TYPE_INT_ARGB)
      val g = domImg.createGraphics

      // Fill background
      g.setColor(Color.WHITE)
      g.fillRect(0, 0, domImg.getWidth, domImg.getHeight)

      // Render responses
      val nRequests = elements.length
      var nResponses = 0
      while (nResponses < nRequests) {
        inbox.receive(1.minute) match {
          case RenderActor.Rendering(s, elem, origin, imgOpt) ⇒
            for (img ← imgOpt) {
              g.drawImage(img, null, spec.map.origin.x + origin.x, spec.map.origin.y + origin.y)
              publish((origin, img))
            }
          nResponses += 1
          setProgress(math.rint(100.0*(step +
              nResponses.toDouble/nRequests)/nSpecs).toInt)
        }
      }

      // Draw element borders
      val transparentBlack = new Color(0, 0, 0, 95)
      for (elem ← elements) {
        g.setColor(transparentBlack)
        for (node ← elem.data.coords) {
          val center = {
            val tmp = spec.map.domainToRoundedPixelPoint(DomainPoint(node.x, node.y))
            PixelPoint(spec.map.origin.x + tmp.x, spec.map.origin.y + tmp.y)
          }
          g.drawLine(center.x - 1, center.y, center.x + 1, center.y)
          g.drawLine(center.x, center.y - 1, center.x, center.y + 1)
        }

        g.setColor(Color.BLACK)
        val rect = {
          val xy0 = spec.map.domainToRoundedPixelPoint(
              DomainPoint(elem.xMin, elem.yMax))
          val xy1 = spec.map.domainToRoundedPixelPoint(
              DomainPoint(elem.xMax, elem.yMin))
          new Rectangle(spec.map.origin.x + xy0.x, spec.map.origin.y + xy0.y,
              xy1.x - xy0.x, xy1.y - xy0.y)
        }
        g.draw(rect)
      }

      val outFile = new File(dir, f"frame_${spec.field}_${step}%04d.png")
      ImageIO.write(domImg, "png", outFile)
    }
  }
}
