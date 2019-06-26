package de.htwg.se.rummi.aview.swing

import java.awt.Color

import de.htwg.se.rummi.aview.swing.RackSortMode.RackSortMode
import de.htwg.se.rummi.controller._
import de.htwg.se.rummi.model.{Ending, Player, RummiSet, Tile}

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

  val grid = new Grid(8, 13)
  val rack = new Rack(4, 13)

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

  rack.fields.foreach(t => listenTo(t))
  grid.fields.foreach(t => listenTo(t))

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
        rack.loadRack(getSortedRackTilesFromController)
        sortButton.text = "Sort Mode: " + playerToSortModeMap(activePlayer)
      }
    }
    case event: RackChangedEvent => {
      println("RackChangedEvent")
      rack.loadRack(getSortedRackTilesFromController)
    }

    case event: FieldChangedEvent => {
      println("FieldChangedEvent")
      grid.update(controller.getPlayingField)
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
      rack.loadRack(getSortedRackTilesFromController)
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
    // Click on a filled field an there is a field selected --> Unselect if same selected and clicket is same, else
    //      deselect the currently selected field and select the clicked field
    else if (clickedField.tileOpt.isDefined && selectedField.isDefined) {
      if (clickedField == selectedField.get) {
        selectedField.get.border = Swing.LineBorder(Color.BLACK, 1)
        selectedField = None
      } else {
        selectedField.get.border = Swing.LineBorder(Color.BLACK, 1)
        selectedField = Some(clickedField)
        clickedField.border = Swing.LineBorder(Color.BLACK, 4)
      }
    }
  }

  def init = {
    rack.loadRack(getSortedRackTilesFromController)
    grid.update(controller.getPlayingField)
  }

  /** *
    * Get the tiles of the rack from the controller and sort them according to the current sorting mode of the active player.
    *
    * @return the sorted list of tiles
    */
  def getSortedRackTilesFromController: List[Tile] = {
    val activePlayer = controller.getActivePlayer

    playerToSortModeMap(activePlayer) match {
      case RackSortMode.NONE => controller.getRack(activePlayer)
      case RackSortMode.COLOR => controller.getRack(activePlayer).sortBy(x => (x.color, x.number))
      case RackSortMode.NUMBER => controller.getRack(activePlayer).sortBy(x => (x.number, x.color))
    }
  }

  /** *
    * Moves a tile from a field to another field.
    *
    * @param fieldTo
    * @param fieldFrom
    * @param selectedTile
    */
  private def moveTile(fieldTo: Field, fieldFrom: Field, selectedTile: Tile): Unit = {

    if (rack.fields.contains(fieldTo)) {
      controller.moveTileToRack(selectedTile)
      return
    }

    // Tile was taken from the middle of a set
    if (grid.getField(fieldFrom.row, fieldFrom.col + 1).isDefined &&
      grid.getField(fieldFrom.row, fieldFrom.col + 1).get.tileOpt.isDefined &&
      grid.getField(fieldFrom.row, fieldFrom.col - 1).isDefined &&
      grid.getField(fieldFrom.row, fieldFrom.col - 1).get.tileOpt.isDefined) {

      val setFrom = grid.getSet(fieldFrom).get
      val setStartField = grid.setsInGrid(setFrom)._1
      val setEndField = grid.setsInGrid(setFrom)._2

      val newSet = new RummiSet(Nil)

      val tilesLeftSet = setFrom.tiles.filter(x => setFrom.tiles.indexOf(x) < setFrom.tiles.indexOf(selectedTile))
      tilesLeftSet.foreach(x => controller.moveTile(x, newSet, Ending.RIGHT))

      grid.setsInGrid += newSet -> (setStartField, grid.getField(fieldFrom.row, fieldFrom.col - 1).get)

      grid.setsInGrid += setFrom -> (grid.getField(fieldFrom.row, fieldFrom.col + 1).get, setEndField)

    }
    else if (grid.getField(fieldFrom.row, fieldFrom.col + 1).isDefined && grid.getField(fieldFrom.row, fieldFrom.col + 1).get.tileOpt.isDefined) {
      val setFrom = grid.getSet(fieldFrom).get
      grid.setsInGrid += setFrom -> (grid.getField(fieldFrom.row, fieldFrom.col + 1).get, grid.setsInGrid(setFrom)._2)
    }
    else if (grid.getField(fieldFrom.row, fieldFrom.col - 1).isDefined && grid.getField(fieldFrom.row, fieldFrom.col - 1).get.tileOpt.isDefined) {
      val setFrom = grid.getSet(fieldFrom).get
      grid.setsInGrid += setFrom -> (grid.setsInGrid(setFrom)._1, grid.getField(fieldFrom.row, fieldFrom.col -1).get)
    }

    grid.getField(fieldTo.row, fieldTo.col).get.setTile(selectedTile)

    val row = fieldTo.row
    val col = fieldTo.col
    if (grid.getField(row, col - 1).isDefined &&
      grid.getField(row, col - 1).get.tileOpt.isDefined &&
      grid.getField(row, col + 1).isDefined &&
      grid.getField(row, col + 1).get.tileOpt.isDefined) {
      // Both neighbor fields are set -> combining the two sets to one

      val leftSet = grid.getSet(grid.getField(row, col - 1).get).get
      val rightSet = grid.getSet(grid.getField(row, col + 1).get).get
      grid.setsInGrid += leftSet -> (grid.getLeftField(leftSet), grid.getField(row, col + rightSet.tiles.size).get)
      controller.moveTile(selectedTile, leftSet, Ending.RIGHT)
      rightSet.tiles.foreach(t => controller.moveTile(t, leftSet, Ending.RIGHT))

    } else if (grid.getField(row, col + 1).isDefined && grid.getField(row, col + 1).get.tileOpt.isDefined) {
      // The field on the right is set
      val a = grid.getField(row, col + 1).get
      val set = grid.getSet(a).get
      grid.setsInGrid += set -> (fieldTo, grid.getRighttField(set))
      controller.moveTile(selectedTile, set, Ending.LEFT)

    } else if (grid.getField(row, col - 1).isDefined && grid.getField(row, col - 1).get.tileOpt.isDefined) {
      // The field on the left is set
      val set = grid.getSet(grid.getField(row, col - 1).get).get
      grid.setsInGrid += set -> (grid.getLeftField(set), fieldTo)
      controller.moveTile(selectedTile, set, Ending.RIGHT)
    }
    else {
      val newSet = new RummiSet(Nil)
      grid.setsInGrid += newSet -> (fieldTo, fieldTo)
      controller.moveTile(selectedTile, newSet, Ending.RIGHT)
    }
  }

}