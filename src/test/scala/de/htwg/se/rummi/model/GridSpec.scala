package de.htwg.se.rummi.model

import org.scalatest.{Matchers, WordSpec}

class GridSpec extends WordSpec with Matchers {

  "A grid when constructed " should {
    "throw an exception if tile indexes are greater than rack size" in {
      a[IllegalArgumentException] should be thrownBy Grid(5, 5, Map.empty + ((2, 6) -> Tile(1, BLUE)))
    }

    "return the acquired tile " in {
      val tile = Tile(1, BLUE)
      val grid = Grid(5, 5, Map.empty + ((2, 3) -> tile))
      grid.getTileAt(2, 3) shouldBe Some(tile)
      grid.getTileAt(20546, 3) shouldBe None
    }

    "return the position of a tile" in {
      val tile = Tile(1, BLUE)
      val grid = Grid(5, 5, Map.empty + ((2, 3) -> tile))
      grid.getTilePosition(tile) shouldBe Some((2, 3))
      grid.getTilePosition(Tile(5, BLUE)) shouldBe None
    }

    "return the number of tiles" in {
      Grid(5, 5, Map.empty + ((2, 3) -> Tile(1, BLUE))).size() shouldBe 1
    }
  }
}
