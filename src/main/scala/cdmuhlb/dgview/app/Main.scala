package cdmuhlb.dgview.app

import java.io.File
import javax.swing.{BorderFactory, UIManager}
import scala.collection.JavaConverters._
import scala.swing.{BorderPanel, MainFrame, Orientation, ProgressBar, SimpleSwingApplication}
import scala.swing.{Action, BoxPanel, Button, ComboBox, FlowPanel, Label, Slider, TextField}
import scala.swing.event.{SelectionChanged, ValueChanged}
import com.typesafe.config.ConfigFactory
import cdmuhlb.dgview.{ContourLinearColorMap, Domain, DomainElement, DomainSeq, DomainPlot}
import cdmuhlb.dgview.{PixelMap, PixelBounds, DomainBounds, RenderSpec}
import cdmuhlb.dgview.{SRgbGrayMap, LabGrayMap, BlackbodyMap, DivergingMap, MshRainbowMap, ReverseMap}
import cdmuhlb.dgview.io.{FhebertDataDir, FhebertDataFile}

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
    val colorCombo = new ComboBox(List(
        LabGrayMap, ReverseMap(LabGrayMap, "Lab gray inverse"),
        DivergingMap.preset1, DivergingMap.preset2, DivergingMap.preset3,
        DivergingMap.preset4, DivergingMap.preset5,
        MshRainbowMap.preset1, MshRainbowMap.preset2
      )) {
      selection.item = plot.getColorMap.map
      reactions += {
        case SelectionChanged(box) ⇒
          assert(box == this)
          plot.setColorMap(plot.getColorMap.withColormap(selection.item))
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
          plot.setColorMap(ContourLinearColorMap(lo, hi, nContours,
              plot.getColorMap.map))
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

    val animationControls = new AnimationControls(plot)

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
          contents += animationControls.button
          contents += animationControls.progressBar
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
