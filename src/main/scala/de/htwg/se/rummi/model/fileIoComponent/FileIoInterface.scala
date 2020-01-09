package de.htwg.se.rummi.model.fileIoComponent

import de.htwg.se.rummi.model.Game

trait FileIoInterface {

  def load: Game
  def save(game: Game) : String

}
