package de.htwg.se.rummi.model

import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.Json

class TileSpec extends WordSpec with Matchers {

  "A tile is a game piece that has a number and a color or is a jocker." should {

    val tileBlueFour = Tile(4, BLUE, false)

    "convert to xml" in {
      tileBlueFour.toXml should be
      <tile>
        <number>4</number>
        <color>BLUE</color>
        <joker>false</joker>
      </tile>
    }

    "convert to json" in {
      Json.toJson(tileBlueFour) should be
        Json.parse("{\"number\":4,\"color\":\"BLUE\",\"joker\":false}")
    }

    "equals compares by reference, not by value" in {
      tileBlueFour equals tileBlueFour should be(true)
      tileBlueFour equals Tile(4, BLUE, false) should be(false)
    }

    "equals with another type should fail" in {
      tileBlueFour equals "some random string" should be(false)
    }

    "toString converts to a ansi colorized output of the number" in {
      val ansiColorBlue = "\u001B[34m"
      val ansiColorReset = "\u001B[0m"
      tileBlueFour.toString should be(ansiColorBlue + "4" + ansiColorReset)
    }

    "if tile is a joker, it should print a white 'J'" in {
      val ansiColorWhite = "\u001B[37m"
      val ansiColorReset = "\u001B[0m"
      Tile(2342, RED, true).toString should be(ansiColorWhite + "J" + ansiColorReset)
    }

  }
}
