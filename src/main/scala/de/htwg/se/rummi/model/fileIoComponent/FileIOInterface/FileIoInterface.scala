package de.htwg.se.sudoku.model.fileIoComponent

import de.htwg.se.rummi.model.Game
import de.htwg.se.rummi.model.gridComponent.GridInterface

trait FileIoInterface {

  def load: Game
  def save(game: Game): Unit

}
