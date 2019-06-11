package de.htwg.se.rummi.aview.swing

import java.awt.Color

import de.htwg.se.rummi.controller.Controller
import de.htwg.se.rummi.model.{Run, Tile}

import scala.swing._
import scala.swing.event.ButtonClicked


/**
  *
  * @param controller
  */
class SwingGui(controller: Controller) extends MainFrame {

  preferredSize = new Dimension(1100, 800)
  title = "Rummikub in Scala"

  var fieldsInGrid: List[Field] = Nil
  var fieldsInRack: List[Field] = Nil

  val RACK_ROWS: Int = 4
  val RACK_COLS: Int = 13

  val GRID_ROWS: Int = 8
  val GRID_COLS: Int = 13


  val grid = new GridPanel(8, 13) {

    preferredSize = new Dimension(GRID_COLS * 11, GRID_ROWS * 11)
    resizable = false

    for (i <- 1 to GRID_ROWS) {
      for (j <- 1 to GRID_COLS) {
        val field = new Field(i, j)
        field.border = Swing.LineBorder(Color.BLACK, 1)
        contents += field
        fieldsInGrid = field :: fieldsInGrid
      }
    }
  }

  menuBar = new MenuBar() {
    contents += new Menu("Menu Title") {
      contents += new MenuItem("Quit")
    }
  }


  val rack: GridPanel = new GridPanel(4, 13) {

    preferredSize = new Dimension(RACK_COLS * 11, RACK_ROWS * 11)
    resizable = false

    for (i <- 1 to RACK_ROWS) {
      for (j <- 1 to RACK_COLS) {
        val field = new Field(i, j)
        field.border = Swing.LineBorder(Color.BLACK, 1)
        contents += field
        fieldsInRack = field :: fieldsInRack
      }
    }
  }

  val center = new BoxPanel(Orientation.Vertical) {

    val sep = new BoxPanel(Orientation.Horizontal)
    sep.

    contents += grid
    contents += rack
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += new Button("Finish")
      contents += new Button("Check")
      contents += new Button("Get Tile")
    }
  }

  contents = new BorderPanel() {
    add(center, BorderPanel.Position.Center)
    add(new Label("Label"), BorderPanel.Position.South)
  }

  fieldsInRack.foreach(t => listenTo(t))
  fieldsInGrid.foreach(t => listenTo(t))

  var selectedField: Option[Field] = Option.empty


  reactions += {
    case ButtonClicked(b) => {

      val clickedField: Field = b.asInstanceOf[Field]


      // Click on a empty field and there is no field selected -> Do nothing
      if (clickedField.tileOpt.isEmpty && selectedField.isEmpty) {

      }
      // Click on a empty field an there is a field selected -> move selected to empty field, unselect
      else if (clickedField.tileOpt.isEmpty && selectedField.isDefined) {
        moveTile(clickedField, selectedField.get, selectedField.get.tileOpt.get)
        selectedField.get.border = Swing.LineBorder(Color.BLACK, 1)
        selectedField = None
      }
      // Click on a filled field an no field selected -> Select field
      else if (clickedField.tileOpt.isDefined && selectedField.isEmpty) {
        selectedField = Some(clickedField)
        clickedField.border = Swing.LineBorder(Color.BLACK, 4)
      }
      // Click on a filled field an there is a field selected --> Unselect if same selected and clicket is same, else Do nothing
      else if (clickedField.tileOpt.isDefined && selectedField.isDefined) {
        if (clickedField == selectedField.get) {
          selectedField.get.border = Swing.LineBorder(Color.BLACK, 1)
          selectedField = None
        }
      }
    }
  }


  def init = {
    val player = controller.getActivePlayer
    val field = controller.getPlayingField
    val rack = controller.getRack(player)

    field.foreach(rummiSet => {
      var row = 1
      var col = 1
      if (rummiSet.isInstanceOf[Run]) {
        val run = rummiSet.asInstanceOf[Run]
        run.tiles.sortBy(t => t.number).foreach(t => {
          val field = getFieldInGrid(row, col).get
          field.setTile(t)
          col += 1
        })
      }
    })

    var row = 1
    var col = 1
    rack.foreach(tile => {
      val field = getFieldInRack(row, col).get
      field.setTile(tile)
      col += 1
      if (col > RACK_COLS) {
        row += 1
        col = 1
      }
    })
  }

  /** *
    * Moves a tile from a field to another field.
    *
    * @param field
    * @param selectedTile
    */
  private def moveTile(fieldTo: Field, fieldFrom: Field, selectedTile: Tile): Unit = {
    fieldTo.setTile(selectedTile)
    fieldFrom.unsetTile()
    // controller.moveTile(selectedTile, Some(new Run(Nil)))
  }

  def isGridField(field: Field): Boolean = {
    return fieldsInGrid.contains(field)
  }

  def getFieldInGrid(x: Int, y: Int): Option[Field] = {
    fieldsInGrid.find(f => (f.x == x && f.y == y))
  }

  def getFieldInRack(x: Int, y: Int): Option[Field] = {
    fieldsInRack.find(f => (f.x == x && f.y == y))
  }


  def setField(x: Int, y: Int, color: Color, text: String): Unit = {
    val field = getFieldInGrid(x, y).get
    field.background = color
    field.text = text
  }
}