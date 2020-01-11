package de.htwg.se.rummi.model.fileIoComponent.xmlFileIo

import de.htwg.se.rummi.model.Game
import de.htwg.se.rummi.model.fileIoComponent.FileIoInterface

import scala.xml.PrettyPrinter

class XmlFileIo extends FileIoInterface{
  override def load: Game = ???

  override def save(game: Game): String = {
    import java.io._
    val pw = new PrintWriter(new File("grid.xml"))
    val prettyPrinter = new PrettyPrinter(120, 4)
    val xml = prettyPrinter.format(gameToXml(game))
    pw.write(xml)
    pw.close
    xml
  }

  def gameToXml(game: Game) = {
    game toXml
  }
}
