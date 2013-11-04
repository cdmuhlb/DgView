package cdmuhlb.dgview.actor

import java.awt.Dimension
import java.awt.image.BufferedImage
import java.util.Comparator
import akka.actor.{Actor, ActorSystem}
import akka.dispatch.{Envelope, UnboundedPriorityMailbox}
import com.typesafe.config.Config
import cdmuhlb.dgview.{DomainBounds, DomainElement, RenderSpec, PixelPoint}

object RenderActor {
  case class Render(seqNum: Int, spec: RenderSpec, elem: DomainElement)
  case class Rendering(seqNum: Int, elem: DomainElement, origin: PixelPoint,
      imgOpt: Option[BufferedImage])
}

class RenderActor extends Actor {
  import RenderActor._
  var maxSeq = 0
  var isMaxSeqSet = false

  def receive = {
    case Render(seqNum, spec, elem) ⇒
      if (!isMaxSeqSet) {
        maxSeq = seqNum
        isMaxSeqSet = true
      }
      if ((seqNum - maxSeq) >= 0) {
        maxSeq = seqNum
        val elemBounds = DomainBounds(elem)
        val pxLo = spec.map.lowerPixelPoint(elemBounds)
        val pxHi = spec.map.upperPixelPoint(elemBounds)
        val width = pxHi.x - pxLo.x + 1
        val height = pxHi.y - pxLo.y + 1
        val imgOpt = if ((width > 0) && (height > 0)) {
          val img = new BufferedImage(width, height,
              BufferedImage.TYPE_INT_ARGB)
          for (iy ← 0 until height; 
               ix ← 0 until width) {
            val xy = spec.map.pixelToDomainPoint(
                PixelPoint(pxLo.x + ix, pxLo.y + iy))
            val z = elem.data.interpolateTo(spec.field, xy.x, xy.y)
            img.setRGB(ix, iy, spec.colorMap.map(z))
          }
          Some(img)
        } else None
        sender ! Rendering(seqNum, elem, pxLo, imgOpt)
      }
  }
}

object RenderComparator extends Comparator[Envelope] {
  def compare(e1: Envelope, e2: Envelope): Int = {
    import RenderActor._
    (e1.message, e2.message) match {
      case (Render(s1, _, _), Render(s2, _, _)) ⇒ s2 - s1
      case (r: Render, other) ⇒ 1
      case (other, r: Render) ⇒ -1
      case (other1, other2) ⇒ 0
    }
  }
}

class RenderMailbox(settings: ActorSystem.Settings, config: Config)
    extends UnboundedPriorityMailbox(RenderComparator)
