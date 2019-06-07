package de.htwg.se.rummi.model

import de.htwg.se.rummi.model.RummiColors.Color

import scala.runtime.ScalaRunTime

case class Tile(number: Int, color: Color, joker: Boolean) {

  def this(number: Int, color: Color) = this(number, color, false)


  override def equals(that: Any): Boolean = {
    that match {
      case t: Tile => t.eq(this)
      case _ => false
    }
  }

  override def productPrefix = {
    if (joker) {
      color.stringInColor("J")
    } else {
      color.stringInColor(number.toString)
    }
  }
}
