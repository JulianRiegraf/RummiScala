package de.htwg.se.rummi.controller.controllerBaseImpl

import de.htwg.se.rummi.controller.ControllerInterface
import de.htwg.se.rummi.model.{Grid, Tile}
import de.htwg.se.rummi.util.Command

class MoveTileCommand(gridFrom: Grid, gridTo: Grid, tile: Tile, newRow: Int, newCol: Int, controller: ControllerInterface) extends Command {

  var position: (Int, Int) = _
  var newFromGrid: Grid = _
  var newToGrid: Grid = _

  override def doStep: Unit = {
    position = gridFrom.getTilePosition(tile).getOrElse(throw new NoSuchElementException("Tile not in grid."))
    val tuple = controller.updateGrids(gridFrom, gridTo, tile, newRow, newCol)
    newFromGrid = tuple._1
    newToGrid = tuple._2
  }

  override def undoStep: Unit = {
    val tuple = controller.updateGrids(newToGrid, newFromGrid, tile, position._1, position._2)
    newToGrid = tuple._1
    newFromGrid = tuple._2
  }

  override def redoStep: Unit = {
    val tuple = controller.updateGrids(newFromGrid, newToGrid, tile, newRow, newCol)
    newFromGrid = tuple._1
    newToGrid = tuple._2
  }
}

