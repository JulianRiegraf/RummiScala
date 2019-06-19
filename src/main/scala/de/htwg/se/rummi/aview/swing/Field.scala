package de.htwg.se.rummi.aview.swing

import java.awt.Color

import de.htwg.se.rummi.model.{RummiColors, Tile}

import scala.swing.Button

class Field(val row: Int, val col: Int) extends Button {
  var tileOpt = Option.empty[Tile]
  val WIDTH = 10
  val HIGHT = 10
  preferredSize = new swing.Dimension(WIDTH, HIGHT)
  minimumSize = new swing.Dimension(WIDTH, HIGHT)
  maximumSize = new swing.Dimension(WIDTH, HIGHT)
  unsetTile()

  def setTile(tile: Tile): Unit = {

    tileOpt = Some(tile)

    background = tile.color match {
      case RummiColors.YELLOW => Color.decode("#FFFF33")
      case RummiColors.GREEN => Color.decode("#90EE90")
      case RummiColors.BLUE => Color.decode("#add8e6")
      case RummiColors.RED => Color.decode("#ff0000")
    }

    text = tile.joker match {
      case true => "J"
      case false => tile.number.toString
    }
  }

  def unsetTile(): Unit = {
    background = Color.WHITE
    text = ""
    tileOpt = None
  }
}
