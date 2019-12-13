package de.htwg.se.rummi.model

import play.api.libs.json._

case class Tile(number: Int, colour: RummiColour, joker : Boolean = false) {

  override def equals(that: Any): Boolean = {
    that match {
      case t: Tile => t.eq(this)
      case _ => false
    }
  }

  override def toString: String = {
    if (joker) {
      WHITE.stringInColor("J")
    } else {
      colour.stringInColor(number.toString)
    }
  }

  def toXml = {
    <tile>
      <number>
        {number}
      </number>
      <color>
        {colour}
      </color>
      <joker>
        {joker}
      </joker>
    </tile>
  }
}

object Tile {

  import play.api.libs.json._

  implicit val tileWrites = Json.writes[Tile]
  implicit val tileReads = Json.reads[Tile]
}

