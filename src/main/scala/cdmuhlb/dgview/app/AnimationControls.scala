package cdmuhlb.dgview.app

import java.awt.Dimension
import java.awt.image.BufferedImage
import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import java.io.{File, FileOutputStream, PipedInputStream, PipedOutputStream}
import javax.imageio.ImageIO
import javax.swing.{BorderFactory, SwingWorker}
import scala.collection.mutable.ListBuffer
import scala.swing.{Dialog, Swing}
import scala.swing.{BoxPanel, Orientation}
import scala.swing.{Action, Button, ButtonGroup, RadioButton}
import scala.swing.{Label, ProgressBar, TextField}
import scala.swing.FileChooser
import com.typesafe.config.ConfigFactory
import cdmuhlb.dgview.{DomainBounds, DomainPlot, PixelBounds, PixelMap, RenderSpec}
import cdmuhlb.dgview.actor.{AnimationWorker, FrameReceiver}
import cdmuhlb.dgview.io.Html5Video
import cdmuhlb.dgview.media.{FrameEncoder, H264Level}

class AnimationControls(plot: DomainPlot) {
  val progressBar = new ProgressBar
  val button = new Button(Action("Save animation") {
    showDialog()
  })

  private def showDialog(): Unit = {
    val popup = new Dialog { dialog ⇒
      title = "Save animation"
      contents = new BoxPanel(Orientation.Vertical) {
        border = BorderFactory.createEmptyBorder(4, 4, 4, 4)
        def hSpacer = Swing.RigidBox(new Dimension(8, 0))
        def vSpacer = Swing.RigidBox(new Dimension(0, 4))

        // Output type variables
        val framesRadio = new RadioButton("Frames (PNG)")
        val gifRadio = new RadioButton("Animated GIF")
        val i420Radio = new RadioButton("Raw video (I420)")
        val mp4Radio = new RadioButton("MP4 video (AVC)")
        framesRadio.selected = true
        gifRadio.enabled = AnimationControls.hasImageMagickConvert
        mp4Radio.enabled = AnimationControls.hasX264
        val radioGroup = new ButtonGroup(framesRadio, gifRadio, i420Radio, mp4Radio)

        // Output location variables
        var outputDir = new File(".")
        val outputPrefixField = new TextField("DgViz")

        // Animation parameters
        val resWidthField = new TextField("640")
        val resHeightField = new TextField("360")
        val framerateField = new TextField("6")

        // Output type panel
        contents += new BoxPanel(Orientation.Horizontal) {
          border = BorderFactory.createTitledBorder("Output type")
          contents ++= List(framesRadio, hSpacer, Swing.HGlue,
              gifRadio, hSpacer, Swing.HGlue, i420Radio, hSpacer, Swing.HGlue,
              mp4Radio)
        }
        contents += vSpacer

        // Output location panel
        contents += new BoxPanel(Orientation.Vertical) {
          border = BorderFactory.createTitledBorder("Output location")
          contents += new BoxPanel(Orientation.Horizontal) {
            val dirLabel = new Label(outputDir.getPath)
            val chooser = new FileChooser {
              fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
            }
            contents ++= List(new Label("Directory: "), dirLabel, hSpacer, Swing.HGlue)
            contents += new Button(Action("Browse") {
              val result = chooser.showSaveDialog(this)
              if (result == FileChooser.Result.Approve) {
                outputDir = chooser.selectedFile
                dirLabel.text = outputDir.getPath
              }
            })
          }
          contents += vSpacer
          contents += new BoxPanel(Orientation.Horizontal) {
            contents += new Label("Filename prefix: ")
            contents += outputPrefixField
          }
        }
        contents += vSpacer

        // Animation paramters panel
        contents += new BoxPanel(Orientation.Horizontal) {
          border = BorderFactory.createTitledBorder("Animation parameters")
          contents ++= List(new Label("Width: "), resWidthField, hSpacer, Swing.HGlue,
              new Label("Height: "), resHeightField, hSpacer, Swing.HGlue,
              new Label("Framerate: "), framerateField)
        }
        contents += Swing.RigidBox(new Dimension(0, 4))

        // Butons
        contents += new BoxPanel(Orientation.Horizontal) {
          val okButton = new Button(Action("Render") {
            // Validate inputs
            val outputPrefix = outputPrefixField.text.trim
            if (outputPrefix.isEmpty) {
              Dialog.showMessage(null, "Invalid filename prefix",
                  "Error: Invalid filename prefix", Dialog.Message.Error)
            } else {
              val (resWidth, resHeight, framerate) = try {
                (resWidthField.text.toInt, resHeightField.text.toInt,
                    framerateField.text.toInt)
              } catch {
                case nfe: NumberFormatException ⇒ (-1, -1, -1)
              }
              if ((resWidth <= 0) || (resHeight <= 0) || (framerate <= 0)) {
                Dialog.showMessage(null, "Invalid animation parameters",
                  "Error: Invalid animation parameters", Dialog.Message.Error)
              } else {
                val frameReceiver = radioGroup.selected match {
                  case Some(`framesRadio`) ⇒ Some(new PngSequence(outputDir, outputPrefix))
                  case Some(`gifRadio`) ⇒ Some(new AnimatedGif(framerate, outputDir, outputPrefix))
                  case Some(`i420Radio`) ⇒ Some(new I420Video(resWidth, resHeight, outputDir, outputPrefix))
                  case Some(`mp4Radio`) ⇒ Some(new Mp4Video(resWidth, resHeight, framerate, outputDir, outputPrefix))
                  case _ ⇒
                    Dialog.showMessage(null, "Invalid output type",
                        "Error: Invalid output type", Dialog.Message.Error)
                    None
                }
                for (receiver ← frameReceiver) renderFrames(resWidth, resHeight, receiver)

                dialog.close()
              }
            }
          })
          dialog.defaultButton = okButton
          val cancelButton = new Button(Action("Cancel") {
            dialog.close()
          })
          contents ++= List(Swing.HGlue, cancelButton, hSpacer, okButton)
        }
      }
      modal = true
      pack()
      setLocationRelativeTo(button)
      visible = true
    }
  }

  def renderFrames(hRes: Int, vRes: Int, receiver: FrameReceiver): Unit = {
    val padding = 8
    val field = plot.getField
    val colorMap = plot.getColorMap
    val specs = plot.doms.domains.values.map(dom ⇒
        RenderSpec(dom, field, colorMap, new PixelMap(
            PixelBounds(hRes, vRes), DomainBounds(dom), padding))).toList
    val worker = new AnimationWorker(specs, receiver)
    worker.addPropertyChangeListener(
      new PropertyChangeListener {
        def propertyChange(evt: PropertyChangeEvent) {
          (evt.getPropertyName, evt.getNewValue) match {
            case ("progress", x: Integer) ⇒
              progressBar.indeterminate = false
              progressBar.value = x
            case ("state", SwingWorker.StateValue.STARTED) ⇒
              progressBar.indeterminate = true
              progressBar.value = 0
              button.enabled = false
            case ("state", SwingWorker.StateValue.DONE) ⇒
              progressBar.indeterminate = false
              progressBar.value = 0
              button.enabled = true
          }
        }
      }
    )
    worker.execute()
  }
}

object AnimationControls {
  def hasImageMagickConvert: Boolean = {
    import scala.sys.process._
    var ans = false
    val imageMagickLogger = ProcessLogger(
        line ⇒ if (line.contains("ImageMagick")) ans = true)
    try {
      "convert --version" ! imageMagickLogger
    } catch {
      case e: Exception ⇒ Unit
    }
    ans
  }

  def hasX264: Boolean = {
    import scala.sys.process._
    val nullLogger = ProcessLogger(_ ⇒ Unit)
    try {
      "x264" ! nullLogger
      true
    } catch {
      case e: Exception ⇒ false
    }
  }
}

class PngSequence(dir: File, prefix: String) extends FrameReceiver {
  assert(dir.isDirectory)
  def receiveFrame(img: BufferedImage, step: Int): Unit = {
    val outFile = new File(dir, f"${prefix}_$step%04d.png")
    ImageIO.write(img, "png", outFile)
  }
  def noMoreFrames(): Unit = Unit
}

class AnimatedGif(fps: Int, dir: File, prefix: String) extends FrameReceiver {
  assert(dir.isDirectory)
  private val filesBuffer = new ListBuffer[File]

  def receiveFrame(img: BufferedImage, step: Int): Unit = {
    val outFile = File.createTempFile("dgview_frame_", ".png")
    filesBuffer += outFile
    ImageIO.write(img, "png", outFile)
  }

  def noMoreFrames(): Unit = {
    import scala.sys.process._
    val delay = math.round(100.0f/fps)
    val files = filesBuffer.result
    val cmd = List("convert", "-delay", delay.toString, "-loop", "0") :::
        (files.map(_.getCanonicalPath) :+ (new File(dir, s"$prefix.gif")).getCanonicalPath)
    try {
      val ret = cmd.!
      if (ret != 0) Dialog.showMessage(null, "Error creating animated GIF",
          s"The `convert` command exited with status $ret", Dialog.Message.Warning)
    } catch {
      case e: Exception ⇒ Dialog.showMessage(null, "Error creating animated GIF",
          s"The `convert` command could not be executed", Dialog.Message.Warning)
    }
    for (file ← files) file.delete()
  }
}

class I420Video(hRes: Int, vRes: Int, dir: File, prefix: String) extends FrameReceiver {
  assert(dir.isDirectory)
  private val encoder = new FrameEncoder(hRes, vRes)
  private val out = new FileOutputStream(new File(dir, s"prefix.i420"))

  def receiveFrame(img: BufferedImage, step: Int): Unit = {
    encoder.encodeFrame(img, out)
  }

  def noMoreFrames(): Unit = {
    out.close()
  }
}

class Mp4Video(hRes: Int, vRes: Int, fps: Int, dir: File, prefix: String) extends FrameReceiver {
  import scala.sys.process._
  assert(dir.isDirectory)
  private val encoder = new FrameEncoder(hRes, vRes)
  // This pipe would not be necessary with Java's ProcessBuilder, but convenient
  //   access to stdout is not available until Java SE 7.
  private val pipeIn = new PipedInputStream(8192)
  private val pipeOut = new PipedOutputStream(pipeIn)

  private val proc = {
    val conf = ConfigFactory.load().getConfig("cdmuhlb.dgview.x264")
    val x264Crf = conf.getString("crf");
    val x264Tune = conf.getString("tune");
    val x264Profile = conf.getString("profile");
    val x264KeyintSeconds = conf.getDouble("keyint-seconds");
    val level = H264Level.findLevel(hRes, vRes, fps)
    val pb = Process(List(
        "x264", "--crf", x264Crf,
        "--preset", "veryslow", "--tune", x264Tune,
        "--fps", fps.toString,
        "--keyint", math.round(x264KeyintSeconds*fps).toString,
        "--profile", x264Profile, "--level", level.level,
        "--vbv-maxrate", (if (x264Profile == "high") level.maxBitrateHigh else level.maxBitrate).toString,
        "--vbv-bufsize", (if (x264Profile == "high") level.maxBitrateHigh else level.maxBitrate).toString,
        "--non-deterministic",
        //"--quiet", "--no-progress",
        "--sar", "1:1", "--overscan", "show",
        "--range", "tv", "--colorprim", "bt709", "--transfer", "bt709",
        "--colormatrix", "bt709", "--chromaloc", "0",
        "--output", (new File(dir, s"$prefix.mp4")).getCanonicalPath,
        "--demuxer", "raw", "--input-csp", "i420", "--input-depth", "8",
        "--input-range", "tv",
        "--input-res", s"${hRes}x${vRes}",
        "-"))
      val pio = new ProcessIO(BasicIO.transferFully(pipeIn, _),
          BasicIO.toStdOut, BasicIO.toStdErr)
      pb.run(pio)
  }

  def receiveFrame(img: BufferedImage, step: Int): Unit = {
    encoder.encodeFrame(img, pipeOut)
  }

  def noMoreFrames(): Unit = {
    pipeOut.close()
    proc.exitValue
  }
}
