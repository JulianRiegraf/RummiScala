package de.htwg.se.rummi.model

import de.htwg.se.rummi.model.RummiColors.{Color}

case class Tile(number: Int, color: Color) extends Ordered[Tile] {

  override def compare(that: Tile): Int = number.compareTo(that.number)
}
