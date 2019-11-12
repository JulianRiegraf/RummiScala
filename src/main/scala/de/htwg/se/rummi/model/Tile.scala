package de.htwg.se.rummi.model

import play.api.libs.json._

case class Tile(number: Int, colour: RummiColour, joker: Boolean) {

  def toJson: JsObject = {
    Json.obj(
      "number" -> JsNumber(number),
      "color" -> JsString(colour.name),
      "joker" -> JsBoolean(joker)
    )
  }


  def this(number: Int, color: RummiColour) = this(number, color, false)


  override def equals(that: Any): Boolean = {
    that match {
      case t: Tile => t.eq(this)
      case _ => false
    }
  }

  override def toString: String = {
    if (joker) {
      colour.stringInColor("J")
    } else {
      colour.stringInColor(number.toString)
    }
  }
}

object Tile {

  import play.api.libs.json._

  implicit val tileWrites = new Writes[Tile] {
    override def writes(o: Tile): JsValue = o.toJson
  }
  implicit val tileReads = Json.reads[Tile]
}

