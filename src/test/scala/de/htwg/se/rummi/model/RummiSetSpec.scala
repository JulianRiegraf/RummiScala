package de.htwg.se.rummi.model

import de.htwg.se.rummi.model.RummiColors._
import org.scalatest.{Matchers, WordSpec}

class RummiSetSpec extends WordSpec with Matchers {

  "A group is made from three or four same-value tiles in distinct colors. A valid Group " should {

    val greenTile1 = new Tile(1, GREEN)
    val blueTile1 = new Tile(1, BLUE)
    val yellowTile1 = new Tile(1, YELLOW)

    val group1 = new RummiSet(greenTile1 :: blueTile1 :: yellowTile1 :: Nil)

      "return true" in {
        group1.isValidGroup() should be(true)
      }
    }

  "A run is composed of three or more, same-colored tiles, in consecutive number order. A Run" when {
    "to be constructed" should {
      "be return true if valid" in {
        val greenTile1 = new Tile(1, GREEN)
        val greenTile2 = new Tile(2, GREEN)
        val greenTile3 = new Tile(3, GREEN)
        val blueTile3 = new Tile(3, BLUE)

        val run1 = new RummiSet(greenTile1 :: greenTile2 :: greenTile3 :: Nil)
        run1.isValidRun() should be(true)

        val run2 = new RummiSet(greenTile1 :: greenTile2 :: blueTile3 :: Nil)
        run2.isValidRun() should be(false)
      }
    }
  }
}