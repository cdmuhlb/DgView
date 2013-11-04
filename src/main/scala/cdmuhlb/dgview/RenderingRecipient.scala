package cdmuhlb.dgview

import java.awt.image.BufferedImage

trait RenderingRecipient {
  def progressImages(images: Seq[(PixelPoint, BufferedImage)],
      spec: RenderSpec): Unit
  def updateImage(newImg: BufferedImage, spec: RenderSpec): Unit
}
