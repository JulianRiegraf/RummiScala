package de.htwg.se.rummi.model

import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.Json


class PlayerSpec extends WordSpec with Matchers {

    "A player " should {

      val player = Player("Player1")

      "have a valid xml representation" in {
        player.toXml should be
        <player>
          <name>Player1</name>
          <inFirstRound>true</inFirstRound>
          <points>0</points>
        </player>
      }

      "have a valid json representation" in {
        player.toJson should be
        Json.parse("{\"name\":\"Player1\",\"inFirstRound\":true,\"points\":0}")
      }

    }
}
