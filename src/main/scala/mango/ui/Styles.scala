package mango.ui

import java.awt.{Dimension, Font}
import scala.swing.{Alignment, Component, Swing}
import scala.swing.Label
import javax.swing.border.EmptyBorder
import scala.swing.Panel
import javax.swing.BorderFactory

trait headerSetting extends Label {
  font = new Font("SansSerif", Font.BOLD, 16)
  horizontalAlignment = Alignment.Left
  border = new EmptyBorder(10, 10, 10, 10)
}

trait labelSetting extends Label {
  font = new Font("SansSerif", Font.BOLD, 14)
  horizontalAlignment = Alignment.Right
  border = new EmptyBorder(10, 10, 10, 20)
}

trait buttonSetting extends Component {
  font = new Font("SansSerif", Font.BOLD, 15)
}

trait panelSetting extends Component {
  
}

trait listSetting extends Component {
  font = new Font("SansSerif", Font.BOLD, 14)
  val mySize = new Dimension(80, 20)
  preferredSize = mySize
}

trait nameListSetting extends Component {
  val mySize = new Dimension(120, 100)
  preferredSize = mySize
  minimumSize = mySize
}

trait fieldSetting extends Component {
  preferredSize = new Dimension(30, 20)
}

trait Padding extends Component {
  border = Swing.EmptyBorder(10)
}

trait GroupedPanel extends Component {
  def setGroupTitle(groupTitle: String) = border = BorderFactory.createTitledBorder(groupTitle)
}