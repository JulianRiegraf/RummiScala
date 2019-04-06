package de.htwg.se.rummi.model

import de.htwg.se.rummi.model.RummiColors._
import org.scalatest.{Matchers, WordSpec}

class RunSpec extends WordSpec with Matchers {

  "A run is composed of three or more, same-colored tiles, in consecutive number order. A Run" when {
    "to be constructed" should {
      "be return true if valid" in {
        val greenTile1 = new Tile(1, GREEN)
        val greenTile2 = new Tile(2, GREEN)
        val greenTile3 = new Tile(3, GREEN)
        val blueTile3 = new Tile(3, BLUE)

        val run1 = new Run(greenTile1 :: greenTile2 :: greenTile3 :: Nil)
        run1.isValid() should be(true)

        val run2 = new Run(greenTile1 :: greenTile2 :: blueTile3 :: Nil)
        run2.isValid() should be(false)
      }
    }
  }
}
