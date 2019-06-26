package de.htwg.se.rummi.aview

import de.htwg.se.rummi.aview.swing.Grid
import de.htwg.se.rummi.model.RummiColor.GREEN
import de.htwg.se.rummi.model.{RummiSet, Tile}
import org.scalatest.{Matchers, WordSpec}

class GridSpec extends WordSpec with Matchers {

  "A grid is the playingfield of the game. When constructed it " should {


    val grid = new Grid(8, 13)

    val set1 = new RummiSet(new Tile(1, GREEN) :: new Tile(2, GREEN) :: Nil)
    val set2 = new RummiSet(new Tile(1, GREEN) :: new Tile(2, GREEN) :: Nil)


    "find a free space in an empty grid" in {

      val x = grid.getFreeSpaceOnGrid(set1).get
      x._1.row should be(1)
      x._1.col should be(1)

      x._2.row should be(1)
      x._2.col should be(2)

    }

    "find a free space between two sets" in {
      grid.setsInGrid += (set1 -> (grid.getField(1, 3).get, grid.getField(1, 4).get))
      grid.setsInGrid += (set2 -> (grid.getField(1, 9).get, grid.getField(1, 10).get))

      val x = grid.getFreeSpaceOnGrid(set1).get
      x._1.row should be(1)
      x._1.col should be(6)

      x._2.row should be(1)
      x._2.col should be(7)
    }

    "find a space between the edge of the grid and a set" in {
      grid.setsInGrid.filter(_ => false)
      grid.setsInGrid += (set1 -> (grid.getField(1, 5).get, grid.getField(1, 6).get))

      val x = grid.getFreeSpaceOnGrid(set2).get
      x._1.row should be(1)
      x._1.col should be(1)

      x._2.row should be(1)
      x._2.col should be(2)
    }

    "find a space between a set and the edge of the grid" in {
      grid.setsInGrid.clear()
      grid.setsInGrid += (set1 -> (grid.getField(1, 2).get, grid.getField(1, 6).get))

      val x = grid.getFreeSpaceOnGrid(set2).get
      x._1.row should be(1)
      x._1.col should be(8)

      x._2.row should be(1)
      x._2.col should be(9)
    }

    "return None if the grid is to small for the set" in {
      val strangeGrid = new Grid(1, 1)
      strangeGrid.getFreeSpaceOnGrid(set1) should be(None)
    }

    "return None if no space is found" in {
      val singleRowGrid = new Grid(1, 5)
      singleRowGrid.setsInGrid += (set1 -> (grid.getField(1, 2).get, grid.getField(1, 3).get))
      singleRowGrid.getFreeSpaceOnGrid(set1) should be(None)
    }

    "gives the set to which a field belongs" in {
      grid.setsInGrid.clear()
      grid.setsInGrid += (set1 -> (grid.getField(1, 2).get, grid.getField(1, 6).get))
      val field = grid.getField(1, 5).get
      grid.getSet(field).get should be(set1)
    }

    "gives the set to which a field belongs for all fields in the set" in {
      grid.setsInGrid.clear()
      grid.setsInGrid += (set1 -> (grid.getField(1, 2).get, grid.getField(1, 6).get))
      for (i <- 2 to 6) {
        val field = grid.getField(1, 5).get
        grid.getSet(field).get should be(set1)
      }
    }

    "gives None if there is no set for this field" in {
      grid.setsInGrid.clear()
      grid.setsInGrid += (set1 -> (grid.getField(1, 2).get, grid.getField(1, 6).get))

      grid.getSet(grid.getField(1, 1).get) should be(None)
      grid.getSet(grid.getField(1, 7).get) should be(None)
    }
  }
}
