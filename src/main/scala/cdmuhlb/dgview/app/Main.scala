package cdmuhlb.dgview.app

import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import java.io.File
import javax.swing.{BorderFactory, SwingWorker, UIManager}
import scala.collection.JavaConverters._
import scala.swing.{BorderPanel, MainFrame, Orientation, ProgressBar, SimpleSwingApplication}
import scala.swing.{Action, BoxPanel, Button, ComboBox, FlowPanel, Label, Slider, TextField}
import scala.swing.FileChooser
import scala.swing.event.{SelectionChanged, ValueChanged}
import com.typesafe.config.ConfigFactory
import cdmuhlb.dgview.{ContourLinearColorMap, DivergingLinearColorMap, Domain, DomainElement, DomainSeq, DomainPlot}
import cdmuhlb.dgview.{PixelMap, PixelBounds, DomainBounds, RenderSpec}
import cdmuhlb.dgview.{ColorMapFactory, BlackbodyFactory, GammaGrayLinearFactory, DivergingLinearFactory, MshRainbowColorMapFactory}
import cdmuhlb.dgview.actor.AnimationWorker
import cdmuhlb.dgview.io.{FhebertDataDir, FhebertDataFile, Html5Video}

object Main extends SimpleSwingApplication {
  // Allow netlib-java implementation to be configured via application.conf
  val netlibConf =  ConfigFactory.load().getConfig("com.github.fommil.netlib")
  for (k ← netlibConf.root().keySet().asScala) {
    System.setProperty("com.github.fommil.netlib." + k, netlibConf.getString(k))
  }

  val conf = ConfigFactory.load().getConfig("dgview")
  val domDir = new File(conf.getString("domain-dir"))
  if (conf.hasPath("laf")) {
    val laf = conf.getString("laf")
    try UIManager.setLookAndFeel(laf)
    catch {
      case e: Exception ⇒ Console.err.println(s"Invalid L&F: $laf")
    }
  }
  val doms = DomainSeq(new FhebertDataDir(domDir))

  def top = new MainFrame {
    title = s"DgView: ${domDir.getCanonicalPath}"
    val pbar = new ProgressBar
    val plot = new DomainPlot(doms, pbar)

    val labField = new Label("Field:")
    val fieldCombo = new ComboBox(doms.fields) {
      selection.item = plot.getField
      reactions += {
        case SelectionChanged(box) ⇒
          assert(box == this)
          plot.setField(selection.item)
      }
      listenTo(selection)
    }

    val labColor = new Label("Colormap:")
    val colorCombo = new ComboBox(List[ColorMapFactory](
        BlackbodyFactory, DivergingLinearFactory, GammaGrayLinearFactory, MshRainbowColorMapFactory)) {
      selection.item = plot.getColorMap.map.getFactory
      reactions += {
        case SelectionChanged(box) ⇒
          assert(box == this)
          plot.setColorMap(plot.getColorMap.withFactory(selection.item))
      }
      listenTo(selection)
    }

    val labTime = new Label("Timestep:")
    val txtTime = new TextField(6) {
      enabled = false
      text = plot.getTimestep.toString
    }

    val lab1 = new Label("Z-min:")
    val txt1 = new TextField(6)
    val lab2 = new Label("Z-max:")
    val txt2 = new TextField(6)
    val lab3 = new Label("Contours:")
    val txt3 = new TextField(3)
    val cmap0 = plot.getColorMap
    txt1.text = cmap0.lo.toString
    txt2.text = cmap0.hi.toString
    txt3.text = cmap0.nContours.toString

    val button = new Button(Action("Update") {
      try {
        val lo = txt1.text.toDouble
        val hi = txt2.text.toDouble
        val nContours = txt3.text.toInt
        if ((hi > lo) && (nContours > 1)) {
          val colorFactory = plot.getColorMap.map.getFactory
          plot.setColorMap(plot.getColorMap.getFactory.createMap(
              lo, hi).withContours(nContours).withFactory(colorFactory))
        } else {
          Console.err.println("Invalid colormap parameters")
        }
      } catch {
        case nfe: NumberFormatException ⇒
          Console.err.println("Invalid colormap parameters")
      }
    })

    val rangeBtn = new Button(Action("Auto") {
      val range = plot.getDom.dataRange(plot.getField)
      val lo = range._1
      val hi = if (math.abs((lo - range._2)/lo) < 0.001) lo*1.001 else range._2
      txt1.text = f"$lo%.4g"
      txt2.text = f"$hi%.4g"
      button.doClick()
    })

    val slider = new Slider {
      min = 0
      max = doms.times.size - 1
      value = 0
      majorTickSpacing = 1
      paintTicks = true
      reactions += {
        case ValueChanged(source) ⇒
          assert(source == this)
          val time = doms.times(value)
          txtTime.text = time.toString
          plot.setTimestep(time)
      }
    }

    val labRes = new Label("Resolution:")
    val labCross = new Label("×")
    val txtH = new TextField(3) { text = "640" }
    val txtV = new TextField(3) { text = "360" }
    val ioBar = new ProgressBar
    val animateButton = new Button(Action("Save animation") {
      val padding = 8
      val (hRes, vRes) = try {
        (txtH.text.toInt, txtV.text.toInt)
      } catch {
        case nfe: NumberFormatException ⇒ (-1, -1)
      }
      if ((hRes < (2*padding+2)) || (vRes < (2*padding+2))) {
        Console.err.println("Invalid animation resolution")
      } else {
        val chooser = new FileChooser {
          fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
        }
        val result = chooser.showSaveDialog(plot)
        if (result == FileChooser.Result.Approve) {
          val dir = chooser.selectedFile
          assert(dir.isDirectory)
          val field = plot.getField
          val colorMap = plot.getColorMap
          val specs = doms.domains.values.map(dom ⇒
              RenderSpec(dom, field, colorMap, new PixelMap(
                  PixelBounds(hRes, vRes), DomainBounds(dom), padding))).toList
          val worker = new AnimationWorker(specs, dir)
          worker.addPropertyChangeListener(
            new PropertyChangeListener {
              def propertyChange(evt: PropertyChangeEvent) {
                (evt.getPropertyName, evt.getNewValue) match {
                  case ("progress", x: Integer) ⇒
                    ioBar.indeterminate = false
                    ioBar.value = x
                  case ("state", SwingWorker.StateValue.STARTED) ⇒
                    ioBar.indeterminate = true
                    ioBar.value = 0
                  case ("state", SwingWorker.StateValue.DONE) ⇒
                    ioBar.indeterminate = false
                    ioBar.value = 0
                    val video = Html5Video(s"frame_${field}_", hRes, vRes, 6)
                    video.writeConversionScript(new File(dir, "make_html5_video.sh"),
                        "animation")
                    println("To animate, run:")
                    println(s"$$ convert -delay 20 -loop 0 '${dir}/frame_${field}_'*.png animation.gif")
                    println("or run:")
                    println(s"$$ cd '$dir'; bash make_html5_video.sh")
                }
              }
            }
          )
          worker.execute()
        }
      }
    })

    contents = new BorderPanel {
      add(new BoxPanel(Orientation.Vertical) {
        border = BorderFactory.createEmptyBorder(0, 8, 0, 8)
        contents += new FlowPanel {
          contents += labField
          contents += fieldCombo
          contents += labColor
          contents += colorCombo
          contents += labTime
          contents += txtTime
        }
        contents += new FlowPanel {
          contents += lab1
          contents += txt1
          contents += lab2
          contents += txt2
          contents += rangeBtn
          contents += lab3
          contents += txt3
          contents += button
        }
        contents += new FlowPanel {
          contents += labRes
          contents += txtH
          contents += labCross
          contents += txtV
          contents += animateButton
          contents += ioBar
        }
        contents += new BorderPanel {
          add(slider, BorderPanel.Position.Center)
        }
      }, BorderPanel.Position.North)
      add(plot, BorderPanel.Position.Center)
      add(pbar, BorderPanel.Position.South)
    }
    defaultButton = button
  }
}
