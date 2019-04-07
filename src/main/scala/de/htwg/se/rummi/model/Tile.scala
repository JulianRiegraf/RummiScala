package de.htwg.se.rummi.model

import de.htwg.se.rummi.model.RummiColors.{Color}

case class Tile(number: Int, color: Color, joker: Boolean) extends Ordered[Tile] {

  def this(number: Int, color: Color) = this(number, color, false)


  override def compare(that: Tile): Int = number.compareTo(that.number)
}
