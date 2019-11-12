package de.htwg.se.rummi.model

case class Player(name: String) {

  var inFirstRound = true
  var points = 0

}

object Player {
  import play.api.libs.json._
  implicit val playerWrites = Json.writes[Player]
  implicit val playerReads = Json.reads[Player]
}