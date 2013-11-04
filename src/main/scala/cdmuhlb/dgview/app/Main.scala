package cdmuhlb.dgview.app

import java.io.File
import javax.swing.UIManager
import scala.swing.{BorderPanel, MainFrame, Orientation, ProgressBar, SimpleSwingApplication}
import scala.swing.{Action, Button, ComboBox, FlowPanel, Label, TextField}
import scala.swing.event.SelectionChanged
import com.typesafe.config.ConfigFactory
import cdmuhlb.dgview.{ContourLinearColorMap, DivergingLinearColorMap, Domain, DomainElement, DomainPlot}
import cdmuhlb.dgview.io.FhebertDataFile

object Main extends SimpleSwingApplication {
  val conf = ConfigFactory.load().getConfig("dgview")
  val filename = conf.getString("domain-file")
  if (conf.hasPath("laf")) {
    val laf = conf.getString("laf")
    try UIManager.setLookAndFeel(laf)
    catch {
      case e: Exception ⇒ Console.err.println(s"Invalid L&F: $laf")
    }
  }
  val dataFile = new FhebertDataFile(new File(filename))
  val elements = dataFile.elements

  def top = new MainFrame {
    title = s"DgView: $filename"
    val pbar = new ProgressBar
    val dom = Domain(elements.map(dge ⇒
        DomainElement(dge.xMin, dge.yMin, dge.xMax, dge.yMax, dge)))
    val plot = new DomainPlot(dom, pbar)

    val combo = new ComboBox(elements.head.data.keys.toList) {
      selection.item = plot.getField
      reactions += {
        case SelectionChanged(box) ⇒
          assert(box == this)
          plot.setField(selection.item)
      }
      listenTo(selection)
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
          plot.setColorMap(ContourLinearColorMap(lo, hi, nContours,
              DivergingLinearColorMap(lo, hi)))
        } else {
          Console.err.println("Invalid colormap parameters")
        }
      } catch {
        case nfe: NumberFormatException ⇒
          Console.err.println("Invalid colormap parameters")
      }
    })

    contents = new BorderPanel {
      add(new FlowPanel {
          contents += combo
          contents += lab1
          contents += txt1
          contents += lab2
          contents += txt2
          contents += lab3
          contents += txt3
          contents += button
        }, BorderPanel.Position.North)
      add(plot, BorderPanel.Position.Center)
      add(pbar, BorderPanel.Position.South)
    }
    defaultButton = button
  }
}
