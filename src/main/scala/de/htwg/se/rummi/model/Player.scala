package de.htwg.se.rummi.model

import play.api.libs.json.{JsObject, Json}

case class Player(name: String, var inFirstRound: Boolean = true, var points: Int = 0) {
  def toXml = {
    <player>
      <name>{name}</name>
      <inFirstRound>{inFirstRound}</inFirstRound>
      <points>{points}</points>
    </player>
  }


  def toJson: JsObject = {
    Json.obj(
      "name" -> name,
      "inFirstRound" -> inFirstRound,
      "points" -> points
    )
  }

  implicit val playerWrites = Json.writes[Game]
  implicit val playerReads = Json.reads[Player]
}