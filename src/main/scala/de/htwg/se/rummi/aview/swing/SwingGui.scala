package de.htwg.se.rummi.aview.swing

import java.awt.Color

import de.htwg.se.rummi.aview.swing.RackSortMode.RackSortMode
import de.htwg.se.rummi.controller._
import de.htwg.se.rummi.model.{Ending, Player, RummiSet, Tile}

import scala.collection.mutable
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

  var playerToSortModeMap = Map[Player, RackSortMode](controller.getActivePlayer -> RackSortMode.COLOR)

  val statusLabel = new Label(controller.statusMessage)
  val playerLabel = new Label("Current Player: " + controller.getActivePlayer.name)

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

  val newGameMenuItem = new MenuItem("New Game")
  listenTo(newGameMenuItem)
  val quitMenuItem = new MenuItem("Quit")
  listenTo(quitMenuItem)

  menuBar = new MenuBar() {
    contents += new Menu("Menu") {
      contents += newGameMenuItem
      contents += quitMenuItem
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


  val sortButton = new Button("Sort Mode: " + playerToSortModeMap(controller.getActivePlayer))
  listenTo(sortButton)


  val south = new GridPanel(1, 3) {
    contents += playerLabel
    contents += statusLabel
    contents += new FlowPanel {
      contents += sortButton
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
      } else if (b == checkButton) {
        if (controller.isValid()) {
          statusLabel.text = "valid"
        } else {
          statusLabel.text = "invalid"
        }
      } else if (b == quitMenuItem) {
        sys.exit(0)
      } else if (b == newGameMenuItem) {
        controller.initGame()
      } else if (b == sortButton) {
        val activePlayer = controller.getActivePlayer
        playerToSortModeMap = playerToSortModeMap + (activePlayer -> RackSortMode.next(playerToSortModeMap(activePlayer)))
        loadRack()
        sortButton.text = "Sort Mode: " + playerToSortModeMap(activePlayer)
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
      playerLabel.text = "Current Player: " + controller.getActivePlayer.name
      if (!playerToSortModeMap.contains(controller.getActivePlayer)) {
        playerToSortModeMap = playerToSortModeMap + (controller.getActivePlayer -> RackSortMode.COLOR)
      }
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

  /** *
    * Get the tiles of the rack from the controller and sort them according to the current sorting mode of the active player.
    *
    * @return the sorted list of tiles
    */
  def sortedRackFromController: List[Tile] = {
    val activePlayer = controller.getActivePlayer

    playerToSortModeMap(activePlayer) match {
      case RackSortMode.NONE => controller.getRack(activePlayer)
      case RackSortMode.COLOR => controller.getRack(activePlayer).sortBy(x => (x.color, x.number))
      case RackSortMode.NUMBER => controller.getRack(activePlayer).sortBy(x => (x.number, x.color))
    }
  }

  /** *
    * Displays the tiles in the rack of the player.
    *
    * @param rack The list of tiles. By default, these are fetched by sortedRackFromController.
    */
  def loadRack(rack: List[Tile] = sortedRackFromController): Unit = {

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

  val setsInGrid = mutable.Map[RummiSet, (Field, Field)]()

  def getLeftField(set: RummiSet): Field = {
    setsInGrid(set)._1
  }

  def getRighttField(set: RummiSet): Field = {
    setsInGrid(set)._2
  }

  def getSet(field: Field): RummiSet = {
    setsInGrid.find(x => {
      val (left, right) = x._2
      left.row == field.row && left.col <= field.col && right.col >= field.col
    }).get._1
  }

  def loadGrid(): Unit = {

    fieldsInGrid.foreach(x => x.unsetTile())
    val field = controller.getPlayingField

    for (s <- field) {
      val leftField = getFieldInGrid(setsInGrid.size + 1, 1).get
      val rightField = getFieldInGrid(setsInGrid.size + 1, 1 + s.tiles.size).get
      setsInGrid.getOrElseUpdate(s, (leftField, rightField))
    }

    for ((set, field) <- setsInGrid.toList.reverse) {

      val row = field._1.row
      var col = field._1.col

      for (i <- 0 to set.tiles.size - 1) {

        val field = getFieldInGrid(row, col) match {
          case Some(f) => f
          case None => throw new NoSuchElementException
        }

        field.setTile(set.tiles(i))
        val x: Int = set.tiles.size - 1

        i match {
          case 0 => field.border = Swing.MatteBorder(2, 2, 2, 0, Color.RED)
          case `x` => field.border = Swing.MatteBorder(2, 0, 2, 2, Color.RED)
          case _ => field.border = Swing.MatteBorder(2, 0, 2, 0, Color.RED)
        }
        col += 1
      }
    }

    println("Sets in Grid")
    for ((s, (lf, rf)) <- setsInGrid) {
      print("\t- (" + lf.row + ", " + lf.col + ") -> (" + rf.row + ", " + rf.col + ")\t")
      s.tiles.foreach(x => print(x + "|"))
      println()
    }

  }

  def isTileOnField(tile: Tile): Boolean = {
    return fieldsInGrid.find(t => t.tileOpt.isDefined && t.tileOpt.get == tile).isDefined
  }

  /** *
    * Moves a tile from a field to another field.
    *
    * @param fieldTo
    * @param fieldFrom
    * @param selectedTile
    */
  private def moveTile(fieldTo: Field, fieldFrom: Field, selectedTile: Tile): Unit = {

    if (fieldsInRack.contains(fieldTo)) {
      controller.moveTileToRack(selectedTile)
      return
    }

    getFieldInGrid(fieldTo.row, fieldTo.col).get.setTile(selectedTile)

    val row = fieldTo.row
    val col = fieldTo.col
    if (getFieldInGrid(row, col + 1).isDefined && getFieldInGrid(row, col + 1).get.tileOpt.isDefined) {
      // The field on the right is set
      val set = getSet(getFieldInGrid(row, col + 1).get)
      setsInGrid += set -> (fieldTo, getRighttField(set))
      controller.moveTile(selectedTile, set, Ending.LEFT)

    } else if (getFieldInGrid(row, col - 1).isDefined && getFieldInGrid(row, col - 1).get.tileOpt.isDefined) {
      // The field on the left is set
      val set = getSet(getFieldInGrid(row, col - 1).get)
      setsInGrid += set -> (getLeftField(set), fieldTo)
      controller.moveTile(selectedTile, set, Ending.RIGHT)
    }
    else {
      val newSet = new RummiSet(Nil)
      setsInGrid += newSet -> (fieldTo, fieldTo)
      controller.moveTile(selectedTile, newSet, Ending.RIGHT)
    }
  }

  def isGridField(field: Field): Boolean = {
    return fieldsInGrid.contains(field)
  }

  def getFieldInGrid(x: Int, y: Int): Option[Field] = {
    fieldsInGrid.find(f => (f.row == x && f.col == y))
  }

  def getFieldInRack(x: Int, y: Int): Option[Field] = {
    fieldsInRack.find(f => (f.row == x && f.col == y))
  }


  def setField(x: Int, y: Int, color: Color, text: String): Unit = {
    val field = getFieldInGrid(x, y).get
    field.background = color
    field.text = text
  }
}