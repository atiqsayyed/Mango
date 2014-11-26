package mango.ui

import java.awt.{ Dimension, Font }
import scala.swing.{ Alignment, BoxPanel, Button, ButtonGroup, Dialog, FlowPanel, Label, Orientation, RadioButton, Separator, Swing, TextField }
import scala.swing.event.{ ButtonClicked, WindowClosing }
import scala.swing.ComboBox
import scala.swing.GridPanel
import javax.swing.BorderFactory
import scala.swing.FormattedTextField
import java.text.Format
import java.text.NumberFormat
import java.awt.ItemSelectable
import scala.swing.event.SelectionChanged
import scala.swing.event.SelectionChanged

class SettingsDialog(setConfig: (Config) => Unit) extends Dialog {

  resizable = false

  private def mkEmptyCell(prefSizeOpt: Option[Dimension] = None) = new Label("") {
    prefSizeOpt foreach { prefSize =>
      preferredSize = prefSize
    }
  }

  private def showMainDialog = {
    ScalaUI.begin
  }

  private def getLimitValue = {
    if (limitField.text != "") limitField.text.toInt
    else 100
  }

  reactions += {
    case WindowClosing(_) =>
      sys.exit(0)
  }

  private var config = Config(None, Config.dbs.head, Config.host, Config.port)

  private val portMap = Map("MongoDB" -> 27017, "CouchDB" -> 5984)

  private val dbList = new ComboBox(Config.dbs) with listSetting {
    listenTo(this.selection)
    reactions += {
      case SelectionChanged(dbList) => {
        config = config.copy(selectedDB = this.selection.item)
        portMap.get(this.selection.item) foreach (port =>
          portField.text = port.toString
        )
        setConfig(config)
      }
    }
  }

  private val hostField = new TextField(Config.host) with fieldSetting
  private val portField = new TextField(Config.port.toString) with fieldSetting
  private val limitField = new FormattedTextField(NumberFormat.getNumberInstance) with fieldSetting {
    text = "1024"
    enabled = false
  }

  private val yesButton = new RadioButton("Yes") with buttonSetting {
    reactions += {
      case ButtonClicked(b) => limitField.enabled = true
    }
  }
  private val noButton = new RadioButton("No") with buttonSetting {
    selected = true
    reactions += {
      case ButtonClicked(b) => limitField.enabled = false
    }
  }

  private val mutex = new ButtonGroup(yesButton, noButton)

  private val okButton = new Button("OK") with buttonSetting {
    reactions += {
      case ButtonClicked(b) => {
        val limit = mutex.selected.get match {
          case `yesButton` => {
            Some(limitField.text.toInt)
          }
          case `noButton` => {
            None
          }
        }
        config = config.copy(docCount = limit, host = hostField.text, port = portField.text.toInt)
        setConfig(config)
        showMainDialog
        SettingsDialog.this.close
      }
    }
  }

  private val settingPanel: BoxPanel = new BoxPanel(Orientation.Vertical) with Padding {
    contents += new GridPanel(3, 2) with GroupedPanel {
      setGroupTitle("Connection Settings")

      contents += new Label("Database") with labelSetting
      contents += dbList

      contents += new Label("Host") with labelSetting
      contents += hostField

      contents += new Label("Port") with labelSetting
      contents += portField

    }

    contents += mkEmptyCell(Some(new Dimension(10, 20)))

    contents += new GridPanel(2, 3) with GroupedPanel {
      setGroupTitle("Analysis")

      contents += new Label("Fetch Limit") with labelSetting
      contents += noButton
      contents += mkEmptyCell()

      contents += mkEmptyCell()
      contents += yesButton
      contents += limitField
    }

    contents += okButton
    title = "Settings"
  }

  contents = settingPanel
  centerOnScreen
}
