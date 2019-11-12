package de.htwg.se.rummi.model.gridComponent.xmlFileIo

import de.htwg.se.rummi.model.Game
import de.htwg.se.sudoku.model.fileIoComponent.FileIoInterface

class XmlFileIo extends FileIoInterface{
  override def load: Game = ???

  override def save(game: Game): String = ???
}
