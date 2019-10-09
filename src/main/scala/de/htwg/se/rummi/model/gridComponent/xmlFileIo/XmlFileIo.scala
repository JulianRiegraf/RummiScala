package de.htwg.se.rummi.model.gridComponent.xmlFileIo

import de.htwg.se.rummi.model.gridComponent.GridInterface
import de.htwg.se.sudoku.model.fileIoComponent.FileIoInterface

class XmlFileIo extends FileIoInterface{
  override def load: GridInterface = ???

  override def save(grid: GridInterface): Unit = ???
}
