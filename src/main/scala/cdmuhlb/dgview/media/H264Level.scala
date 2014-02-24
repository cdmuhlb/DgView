package cdmuhlb.dgview.media

import com.typesafe.config.ConfigFactory

case class H264Level(level: String, maxPixels: Int, maxPixelRate: Int,
    maxBitrate: Int, maxBitrateHigh: Int)

object H264Level {
  val Level1 = H264Level("1", 25344, 380160, 64, 80)
  //val Level1b = H264Level("1b", 25344, 380160, 128, 160)
  val Level1_1 = H264Level("1.1", 101376, 768000, 192, 240)
  val Level1_2 = H264Level("1.2", 101376, 1536000, 384, 480)
  //val Level1_3 = H264Level("1.3", 101376, 3041280, 768, 960)
  val Level2 = H264Level("2", 101376, 3041280, 2000, 2500)
  val Level2_1 = H264Level("2.1", 202752, 5068800, 4000, 5000)
  val Level2_2 = H264Level("2.2", 414720, 5184000, 4000, 5000)
  val Level3 = H264Level("3", 414720, 10368000, 10000, 12500)
  val Level3_1 = H264Level("3.1", 921600, 27648000, 14000, 17500)
  val Level3_2 = H264Level("3.2", 1310720, 55296000, 20000, 25000)
  //val Level4 = H264Level("4", 2097152, 62914560, 20000, 25000)
  val Level4_1 = H264Level("4.1", 2097152,  62914560,  50000,  62500)
  val Level4_2 = H264Level("4.2", 2228224, 133693440, 50000, 62500)
  val Level5 = H264Level("5", 5652480, 150994944, 135000, 168750)
  val Level5_1 = H264Level("5.1", 9437184, 251658240, 240000, 300000)

  private val levels = List(Level1, Level1_1, Level1_2, Level2, Level2_1,
      Level2_2, Level3, Level3_1, Level3_2, Level4_1, Level4_2, Level5,
      Level5_1)

  // TODO: Implement this properly
  def findLevel(width: Int, height: Int, fps: Int): H264Level = {
    val nPixels = width*height
    val pixelRate = nPixels*fps
    // TODO: Search for ideal level
    if ((nPixels <= Level4_1.maxPixels) && (pixelRate <= Level4_1.maxPixelRate)) {
      Level4_1
    } else Level5_1
  }
}
