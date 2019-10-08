package de.htwg.se.sudoku.model.fileIoComponent

import de.htwg.se.rummi.model.gridComponent.GridInterface

trait FileIoInterface {

  def load: GridInterface
  def save(grid: GridInterface): Unit

}
