package cdmuhlb.dgview.app

import java.awt.Dimension
import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import java.io.File
import javax.swing.{BorderFactory, SwingWorker}
import scala.swing.{Dialog, Swing}
import scala.swing.{BoxPanel, Orientation}
import scala.swing.{Action, Button, ButtonGroup, RadioButton}
import scala.swing.{Label, ProgressBar, TextField}
import scala.swing.FileChooser
import cdmuhlb.dgview.{DomainBounds, DomainPlot, PixelBounds, PixelMap, RenderSpec}
import cdmuhlb.dgview.actor.AnimationWorker
import cdmuhlb.dgview.io.Html5Video

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
        i420Radio.enabled = false
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
                radioGroup.selected match {
                  case Some(`framesRadio`) ⇒
                    renderPngFrames(resWidth, resHeight, outputDir, outputPrefix)
                  case Some(`gifRadio`) ⇒
                    renderAnimatedGif(resWidth, resHeight, framerate, outputDir, outputPrefix)
                  case Some(`i420Radio`) ⇒ println("Rendering raw I420")
                  case Some(`mp4Radio`) ⇒
                    renderMP4Video(resWidth, resHeight, framerate, outputDir, outputPrefix)
                  case _ ⇒ Dialog.showMessage(null, "Invalid output type",
                                "Error: Invalid output type", Dialog.Message.Error)
                }
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

  def renderPngFrames(hRes: Int, vRes: Int, dir: File, prefix: String): Unit = {
    val padding = 8
    assert(dir.isDirectory)
    val field = plot.getField
    val colorMap = plot.getColorMap
    val specs = plot.doms.domains.values.map(dom ⇒
        RenderSpec(dom, field, colorMap, new PixelMap(
            PixelBounds(hRes, vRes), DomainBounds(dom), padding))).toList
    val worker = new AnimationWorker(specs, dir)
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

  def renderAnimatedGif(hRes: Int, vRes: Int, fps: Int, dir: File, prefix: String): Unit = {
    renderPngFrames(hRes, vRes, dir, prefix)
    val delay = math.round(100.0f/fps)
    println("To animate, run:")
    println(s"$$ convert -delay 20 -loop 0 '$dir/frame_${plot.getField}_'*.png $prefix.gif")
  }

  def renderMP4Video(hRes: Int, vRes: Int, fps: Int, dir: File, prefix: String): Unit = {
    renderPngFrames(hRes, vRes, dir, prefix)
    val video = Html5Video(s"frame_${plot.getField}_", hRes, vRes, fps)
    video.writeConversionScript(new File(dir, "make_html5_video.sh"), prefix)
    println("To animate, run:")
    println(s"$$ cd '$dir'; bash make_html5_video.sh")
  }
}
