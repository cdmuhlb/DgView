package cdmuhlb.dgview.media

import java.awt.image.BufferedImage
import java.io.{File, OutputStream}
import cdmuhlb.dgview.color.{Bt709Utils, SRgbUtils}

class FrameEncoder(val width: Int, val height: Int) {
    private val nPixels = width*height;
    private val subWidth = (width - 1)/2 + 1
    private val subHeight = (height - 1)/2 + 1
    private val subNPixels = subWidth*subHeight

    private val rgbBuf = new Array[Int](nPixels)

    private val rBuf = new Array[Float](nPixels)
    private val gBuf = new Array[Float](nPixels)
    private val bBuf = new Array[Float](nPixels)
    private val subRBuf = new Array[Float](subNPixels)
    private val subGBuf = new Array[Float](subNPixels)
    private val subBBuf = new Array[Float](subNPixels)

    private val yBytes = new Array[Byte](nPixels)
    private val cbBytes = new Array[Byte](subNPixels)
    private val crBytes = new Array[Byte](subNPixels)

  def encodeFrame(img: BufferedImage, out: OutputStream): Unit = {
    assert(img.getWidth == width);
    assert(img.getHeight == height);
    img.getRGB(0, 0, width, height, rgbBuf, 0, width);

    // Extract linear RGB
    for (i ← 0 until nPixels) {
      val c = rgbBuf(i)
      bBuf(i) = SRgbUtils.reverseTransfer((c & 0xff) / 255.0f)
      gBuf(i) = SRgbUtils.reverseTransfer(((c >>> 8) & 0xff) / 255.0f)
      rBuf(i) = SRgbUtils.reverseTransfer(((c >>> 16) & 0xff) / 255.0f)
    }

    // Compute luma
    for (i ← 0 until nPixels) {
      val r = Bt709Utils.forwardTransfer(rBuf(i))
      val g = Bt709Utils.forwardTransfer(gBuf(i))
      val b = Bt709Utils.forwardTransfer(bBuf(i))
      yBytes(i) = Bt709Utils.encodeY(Bt709Utils.eY(r, g, b)).toByte
    }

    // TODO: Can invoke a non-blocking write of yBytes here

    // Subsample linear RGB
    subsampleLoc0(rBuf, subRBuf)
    subsampleLoc0(gBuf, subGBuf)
    subsampleLoc0(bBuf, subBBuf)

    // Compute chroma
    for (i ← 0 until subNPixels) {
      val r = Bt709Utils.forwardTransfer(subRBuf(i))
      val g = Bt709Utils.forwardTransfer(subGBuf(i))
      val b = Bt709Utils.forwardTransfer(subBBuf(i))
      val eY = Bt709Utils.eY(r, g, b)
      cbBytes(i) = Bt709Utils.encodeC(Bt709Utils.ePb(b, eY)).toByte
      crBytes(i) = Bt709Utils.encodeC(Bt709Utils.ePr(r, eY)).toByte
    }

    // Write output
    out.write(yBytes)
    out.write(cbBytes)
    out.write(crBytes)
  }

  private def subsampleLoc0(src: Array[Float], dst: Array[Float]): Unit = {
    assert(src.length == nPixels);
    assert(dst.length == subNPixels);
    // Assumes that width and height are even

    for (i ← 0 until subHeight) {
      val idxAbove = 2*i*width
      val idxBelow = (2*i + 1)*width
      val dstIdx = i*subWidth
      assert((2*i + 1) < height)

      // First column
      dst(dstIdx) = 0.25f*(
            src(idxAbove) + src(idxAbove+1) +
            src(idxBelow) + src(idxBelow+1))

      // Remaining columns
      for (j ← 1 until subWidth) {
        val jj = 2*j
        dst(dstIdx+j) = 0.125f*(
            src(idxAbove+jj-1) + 2.0f*src(idxAbove+jj) + src(idxAbove+jj+1) +
            src(idxBelow+jj-1) + 2.0f*src(idxBelow+jj) + src(idxBelow+jj+1))
      }
    }
  }
}
