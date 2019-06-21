package de.htwg.se.rummi.aview.swing

import java.awt.Color

import de.htwg.se.rummi.model.Tile

import scala.swing.{Dimension, GridPanel, Swing}

class Rack(val ROWS: Int, val COLS: Int) extends GridPanel(rows0 = ROWS, cols0 = COLS) {

  preferredSize = new Dimension(COLS * 11, ROWS * 11)
  var fields: List[Field] = Nil

  for (i <- 1 to ROWS) {
    for (j <- 1 to COLS) {
      val field = new Field(i, j)
      field.border = Swing.LineBorder(Color.BLACK, 1)
      contents += field
      fields = field :: fields
    }
  }

  def getFieldInRack(x: Int, y: Int): Option[Field] = {
    fields.find(f => (f.row == x && f.col == y))
  }

  /** *
    * Displays the tiles in the rack of the player.
    *
    * @param tiles The list of tiles. By default, these are fetched by sortedRackFromController.
    */
  def loadRack(tiles: List[Tile]): Unit = {

    fields.foreach(x => x.unsetTile())

    var row = 1
    var col = 1
    tiles.foreach(tile => {
      val field = getFieldInRack(row, col).get
      field.setTile(tile)
      col += 1
      if (col > COLS) {
        row += 1
        col = 1
      }
    })
  }

}
