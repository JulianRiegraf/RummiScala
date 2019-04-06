package de.htwg.se.rummi.model

import de.htwg.se.rummi.model.RummiColors._
import org.scalatest.{Matchers, WordSpec}

class GroupSpec extends WordSpec with Matchers {

  "A group is made from three or four same-value tiles in distinct colors. A valid Group " should {

    val greenTile1 = new Tile(1, GREEN)
    val blueTile1 = new Tile(1, BLUE)
    val yellowTile1 = new Tile(1, YELLOW)

    val group1 = new Group(greenTile1 :: blueTile1 :: yellowTile1 :: Nil)

      "return true" in {
        group1.isValid() should be(true)
      }
    }
}