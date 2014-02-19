package cdmuhlb.dgview.actor

import java.awt.Dimension
import java.awt.image.BufferedImage
import java.util.Comparator
import akka.actor.{Actor, ActorSystem}
import akka.dispatch.{Envelope, UnboundedPriorityMailbox}
import com.typesafe.config.Config
import cdmuhlb.dgview.{DomainBounds, DomainElement, RenderSpec, PixelPoint}

/** A message wrapper attaching a sequence number to another message.
  *
  * The sequence number represents the priority of the message.  If a
  * recipient has already processed messages with higher sequence numbers, it
  * may choose to discard this request.
  *
  * @constructor Create a new SequencedMessage message
  * @param seqNum sequence number for the request
  * @param msg message
  */
case class SequencedMessage(seqNum: Int, msg: Any)

object RenderActor {
  /** A message requesting that an element be rendered to a `BufferedImage`
    * according to a [[RenderSpec]].  Responses to this message
    * should be instances of [[Rendering]].
    *
    * The sequence number represents the priority of this message.  If a
    * recipient has already processed messages with higher sequence numbers, it
    * may choose to discard this request.
    *
    * Thread safety: Instances of this class are immutable and therefore
    * thread-safe in all contexts.  Subclasses must respect the immutability of
    * this class's interface.
    *
    * @constructor Create a new Render message
    * @param spec specification for the desired rendering
    * @param elem spectral element whose data is to be rendered
    */
  case class Render(spec: RenderSpec, elem: DomainElement)

  /** A message containing a rendered image (in the form of a `BufferedImage`),
    * intended to be sent in response to a rendering request.
    *
    * Thread safety: Instances of this class are meant to be effectively
    * immutable during their lifetime while representing a transfer of ownership
    * of mutable data.  In particular, the thread constructing this class must
    * not mutate the image once the message has been constructed, must only
    * publish this message to a single recipient, and must not access or publish
    * the image once this message has been published.  Recipients of this class
    * assume full responsibility for synchronizing access to the image should
    * they choose to publish it further.  Subclasses must respect the effective
    * immutability of this class's interface.
    *
    * @constructor Create a new Rendering message
    * @param seqNum sequence number of the request being responded to
    * @param elem spectral element whose data has been rendered
    * @param origin location of the upper-left corner of the rendered image
    *                relative to that of the global image
    * @param imgOpt [[scala.Option]] containing the rendered image, or
    *                [[scala.None]] if the image could not be rendered
    */
  case class Rendering(seqNum: Int, elem: DomainElement, origin: PixelPoint,
      imgOpt: Option[BufferedImage])
}

class RenderActor extends Actor {
  import RenderActor._
  var maxSeq = 0
  var isMaxSeqSet = false

  def receive = {
    case SequencedMessage(seqNum, Render(spec, elem)) ⇒
      if (!isMaxSeqSet) {
        maxSeq = seqNum
        isMaxSeqSet = true
      }
      if ((seqNum - maxSeq) >= 0) {
        maxSeq = seqNum
        val (origin, imgOpt) = render(spec, elem)
        sender ! Rendering(seqNum, elem, origin, imgOpt)
      }
    case Render(spec, elem) ⇒
      val (origin, imgOpt) = render(spec, elem)
      sender ! Rendering(0, elem, origin, imgOpt)
  }

  def render(spec: RenderSpec, elem: DomainElement):
      (PixelPoint, Option[BufferedImage]) = {
    val elemBounds = DomainBounds(elem)
    val pxLo = spec.map.lowerPixelPoint(elemBounds)
    val pxHi = spec.map.upperPixelPoint(elemBounds)
    val width = pxHi.x - pxLo.x + 1
    val height = pxHi.y - pxLo.y + 1
    val imgOpt = if ((width > 0) && (height > 0)) {
      val img = new BufferedImage(width, height,
          BufferedImage.TYPE_INT_ARGB)

      val xs = (0 until width).map(ix ⇒ spec.map.pixelToDomainPoint(
            PixelPoint(pxLo.x + ix, pxLo.y)).x).toVector
      val ys = (0 until height).map(iy ⇒ spec.map.pixelToDomainPoint(
            PixelPoint(pxLo.x, pxLo.y + iy)).y).toVector
      val zs = elem.data.interpolateToGrid(spec.field, xs, ys)
      for (iy ← 0 until height;
           ix ← 0 until width) {
        val z = zs(iy)(ix)
        img.setRGB(ix, iy, spec.colorMap.mapToArgb(z))
      }
      Some(img)
    } else None
    (pxLo, imgOpt)
  }
}

object SequenceComparator extends Comparator[Envelope] {
  def compare(e1: Envelope, e2: Envelope): Int = {
    import RenderActor._
    (e1.message, e2.message) match {
      case (SequencedMessage(s1, _), SequencedMessage(s2, _)) ⇒ s2 - s1
      case (r: SequencedMessage, other) ⇒ 1
      case (other, r: SequencedMessage) ⇒ -1
      case (other1, other2) ⇒ 0
    }
  }
}

class RenderMailbox(settings: ActorSystem.Settings, config: Config)
    extends UnboundedPriorityMailbox(SequenceComparator)
