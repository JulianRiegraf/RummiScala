package de.htwg.se.rummi.aview.swing

import java.awt.Color

import de.htwg.se.rummi.controller._
import de.htwg.se.rummi.model.{RummiColors, Tile}

import scala.swing._
import scala.swing.event.ButtonClicked


/**
  *
  * @param controller
  */
class SwingGui(controller: Controller) extends MainFrame {

  listenTo(controller)

  preferredSize = new Dimension(1100, 800)
  title = "Rummikub in Scala"

  var fieldsInGrid: List[Field] = Nil
  var fieldsInRack: List[Field] = Nil

  val RACK_ROWS: Int = 4
  val RACK_COLS: Int = 13

  val GRID_ROWS: Int = 8
  val GRID_COLS: Int = 13

  val finishButton = new Button("Finish")
  listenTo(finishButton)
  val getTileButton = new Button("Get Tile")
  listenTo(getTileButton)
  val checkButton = new Button("Check")
  listenTo(checkButton)

  val statusLabel = new Label(controller.statusMessage)
  val playerLabel = new Label("Player: " + controller.getActivePlayer.name)

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

    contents += grid
    contents += rack
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += finishButton
      contents += checkButton
      contents += getTileButton
    }
  }

  val south = new GridPanel(1, 3) {
    contents += playerLabel
    contents += statusLabel
    contents += new FlowPanel {
      contents += new Label("Sort by")
      contents += Button("Color") {
        loadRack(controller.getRack(controller.getActivePlayer).sortBy(x => (x.color, x.number)))
      }
      contents += Button("Number"){
        loadRack(controller.getRack(controller.getActivePlayer).sortBy(x => (x.number, x.color)))
      }
    }
  }


  contents = new BorderPanel() {
    add(center, BorderPanel.Position.Center)
    add(south, BorderPanel.Position.South)
  }

  fieldsInRack.foreach(t => listenTo(t))
  fieldsInGrid.foreach(t => listenTo(t))

  var selectedField: Option[Field] = Option.empty


  reactions += {
    case ButtonClicked(b) => {

      if (b.isInstanceOf[Field]) {
        val clickedField: Field = b.asInstanceOf[Field]
        fieldClicked(clickedField)
      } else if (b == getTileButton) {
        controller.draw()
      } else if (b == finishButton) {
        controller.switchPlayer()
      }
    }
    case event: RackChangedEvent => {
      println("RackChangedEvent")
      loadRack()
    }

    case event: FieldChangedEvent => {
      println("FieldChangedEvent")
      loadGrid()
    }

    case event: ValidStateChangedEvent => {
      println("ValidStateChangedEvent")
      if (controller.isValid()) {
        finishButton.enabled = true
      } else {
        finishButton.enabled = false
      }
    }

    case event: PlayerSwitchedEvent => {
      println("--- PlayerSwitchedEvent ---")

      loadRack()
    }

    case event: WinEvent => {
      grid.enabled = false
    }

    case event: StatusMessageChangedEvent => {
      statusLabel.text = controller.statusMessage
    }
  }


  private def fieldClicked(clickedField: Field) = {
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

  def init = {
    loadRack()
    loadGrid
  }

  def loadRack(rack: List[Tile] = controller.getRack(controller.getActivePlayer)): Unit = {

    fieldsInRack.foreach(x => x.unsetTile())

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

  def loadGrid(): Unit = {

    fieldsInGrid.foreach(x => x.unsetTile())

    val player = controller.getActivePlayer
    val field = controller.getPlayingField

    var row = 1
    var col = 1

    for (s <- field) {
      s.tiles = s.tiles.sortBy(_.number)
      for (t <- s.tiles) {
        getFieldInGrid(row, col).get.setTile(t)
        col += 1
      }
      row += 1
      col = 1
    }
  }

  /** *
    * Moves a tile from a field to another field.
    *
    * @param field
    * @param selectedTile
    */
  private def moveTile(fieldTo: Field, fieldFrom: Field, selectedTile: Tile): Unit = {

    if (fieldsInRack.contains(fieldTo)) {
      controller.moveTileToRack(selectedTile)
      return
    }

    val p = controller.getPlayingField
    println("p.size: " + p.size)
    println("fieldTo.x - 1 : " + (fieldTo.x - 1))
    if (p.size > 0 && p.size > fieldTo.x - 1) {
      controller.moveTile(selectedTile, Some(p(fieldTo.x - 1)))
    } else {
      controller.moveTile(selectedTile, None)
    }
    //fieldTo.setTile(selectedTile)
    //fieldFrom.unsetTile()
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