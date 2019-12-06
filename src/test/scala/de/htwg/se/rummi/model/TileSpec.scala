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
      tileBlueFour.toJson should be
        Json.parse("{\"number\":4,\"color\":\"BLUE\",\"joker\":false}")
    }

    "equals compares by reference, not by value" in {
      tileBlueFour equals tileBlueFour should be(true)
      tileBlueFour equals Tile(4, BLUE, false) should be(false)
    }

    "equals with another type should fail" in {
      tileBlueFour equals "some random string" should be(false)
    }

  }
}
