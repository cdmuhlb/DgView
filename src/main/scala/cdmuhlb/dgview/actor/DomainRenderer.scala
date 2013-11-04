package cdmuhlb.dgview.actor

import java.awt.Dimension
import java.awt.image.BufferedImage
import javax.swing.SwingWorker
import scala.concurrent.duration._
import akka.actor.{ActorSystem, Inbox, Props}
import cdmuhlb.dgview.{DomainPlot, PixelPoint, RenderSpec}

object RenderSystem {
  val system = ActorSystem("RenderSystem")
  val actor = system.actorOf(Props[RenderActor].withDispatcher(
    "render-dispatcher"))
  for (i ← 1 until Runtime.getRuntime().availableProcessors) {
    system.actorOf(Props[RenderActor].withDispatcher("render-dispatcher"))
  }
}

class DomainRenderer(plot: DomainPlot) {
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
            RenderActor.Render(seqNum, spec, elem))
      }

      val domImg = new BufferedImage(spec.map.bounds.width,
          spec.map.bounds.height, BufferedImage.TYPE_INT_ARGB)
      val g = domImg.createGraphics

      val nRequests = elements.length
      var nResponses = 0
      while (nResponses < nRequests) {
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
        plot.progressImages(chunks, spec)
      }
    }

    override protected def done(): Unit = {
      if (!isCancelled) {
        plot.updateImage(get, spec)
      }
    }
  }
}
