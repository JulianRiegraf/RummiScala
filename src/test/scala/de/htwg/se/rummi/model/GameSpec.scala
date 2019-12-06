package de.htwg.se.rummi.model

import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.Json

class GameSpec extends WordSpec with Matchers {

  "A game" should {

    val game = Game("player1" :: "player2" :: Nil)

    "some testing" in {

    }


  }
}
