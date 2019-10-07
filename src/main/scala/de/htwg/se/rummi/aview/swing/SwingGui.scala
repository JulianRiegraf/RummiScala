package de.htwg.se.rummi.aview.swing

import java.awt.Color

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

  val finishButton = new Button("Finish")
  listenTo(finishButton)
  val getTileButton = new Button("Get Tile")
  listenTo(getTileButton)
  val checkButton = new Button("Check")
  listenTo(checkButton)

  val statusLabel = new Label(controller.statusMessage)
  val playerLabel = new Label("Current Player: " + controller.activePlayer.name)

  val grid = new SwingGrid(8, 13)
  val rack = new SwingGrid(4, 13)

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

  val sortButton = new Button("Sort")
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

      } else if (b == quitMenuItem) {
        sys.exit(0)
      } else if (b == newGameMenuItem) {
        controller.initGame()
      } else if (b == sortButton) {
        controller.sortRack()
      }
    }

    case event: FieldChangedEvent => {
      grid.displayGrid(controller.field)
      rack.displayGrid(controller.getRack(controller.activePlayer))
    }

    case event: ValidStateChangedEvent => {
      if (controller.isValidField) {
        finishButton.enabled = true
      } else {
        finishButton.enabled = false
      }
    }

    case event: PlayerSwitchedEvent => {
      playerLabel.text = "Current Player: " + controller.activePlayer.name
      rack.displayGrid(controller.getRack(controller.activePlayer))
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

      moveTile(clickedField)

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

  private def moveTile(clickedField: Field) = {
    if (rack.containsField(selectedField.get) && grid.containsField(clickedField)) {
      controller.moveTileFromRackToField(selectedField.get.tileOpt.get, clickedField.row, clickedField.col)
    }

    if (grid.containsField(selectedField.get) && rack.containsField(clickedField)) {
      controller.moveTileFromFieldToRack(selectedField.get.tileOpt.get, clickedField.row, clickedField.col)
    }

    if (rack.containsField(selectedField.get) && rack.containsField(clickedField)) {
      controller.moveTileWithinRack(selectedField.get.tileOpt.get, clickedField.row, clickedField.col)
    }

    if (grid.containsField(clickedField) && grid.containsField(selectedField.get)) {
      controller.moveTileWithinField(selectedField.get.tileOpt.get, clickedField.row, clickedField.col)
    }
  }

  def init = {
    rack.displayGrid(controller.getRack(controller.activePlayer))
    grid.displayGrid(controller.field)
  }

}