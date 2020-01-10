package de.htwg.se.rummi.controller.controllerBaseImpl

import de.htwg.se.rummi.util.Command

class SortCommand(row: Int, col: Int, value: Int, controller: Controller) extends Command {
  override def doStep: Unit = {}

  override def undoStep: Unit = {}

  override def redoStep: Unit = {}
}