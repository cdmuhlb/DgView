package cdmuhlb.dgview.app

import java.io.File
import javax.swing.{BorderFactory, UIManager}
import scala.swing.{BorderPanel, MainFrame, Orientation, ProgressBar, SimpleSwingApplication}
import scala.swing.{Action, BoxPanel, Button, ComboBox, FlowPanel, Label, Slider, TextField}
import scala.swing.event.{SelectionChanged, ValueChanged}
import com.typesafe.config.ConfigFactory
import cdmuhlb.dgview.{ContourLinearColorMap, DivergingLinearColorMap, Domain, DomainElement, DomainSeq, DomainPlot}
import cdmuhlb.dgview.{ColorMapFactory, BlackbodyFactory, GammaGrayLinearFactory, DivergingLinearFactory}
import cdmuhlb.dgview.io.{FhebertDataDir, FhebertDataFile}

object Main extends SimpleSwingApplication {
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
        BlackbodyFactory, DivergingLinearFactory, GammaGrayLinearFactory)) {
      selection.item = plot.getColorMap.map.getFactory
      reactions += {
        case SelectionChanged(box) ⇒
          assert(box == this)
          plot.setColorMap(plot.getColorMap.withFactory(selection.item))
      }
      listenTo(selection)
    }

    val labTime = new Label("Timestep:")
    val txtTime = new TextField(3) {
      enabled = false
      text = plot.getTimestep.toString
    }

    val lab1 = new Label("Z-min:")
    val txt1 = new TextField(4)
    val lab2 = new Label("Z-max:")
    val txt2 = new TextField(4)
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
          contents += lab3
          contents += txt3
          contents += button
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
