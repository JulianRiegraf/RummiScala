package de.htwg.se.rummi.aview

import de.htwg.se.rummi.controller.controllerBaseImpl.Controller
import de.htwg.se.rummi.model._
import de.htwg.se.rummi.model.{Grid, RummiSet, Tile}
import org.scalatest.{Matchers, WordSpec}

class GridSpec extends WordSpec with Matchers {

  "A grid is the playingfield of the game. When constructed it " should {
    val grid = new Grid(8, 13, Map.empty)

    val set1 = new RummiSet(new Tile(1, GREEN) :: new Tile(2, GREEN) :: Nil)
    val set2 = new RummiSet(new Tile(1, GREEN) :: new Tile(2, GREEN) :: Nil)


    "find a free space in an empty grid" in {


    }

    "find a free space between two sets" in {

    }

    "find a space between the edge of the grid and a set" in {

    }

    "find a space between a set and the edge of the grid" in {

    }

    "return None if the grid is to small for the set" in {

    }

    "return None if no space is found" in {

    }

    "gives the set to which a field belongs" in {

    }

    "gives the set to which a field belongs for all fields in the set" in {

    }

    "gives None if there is no set for this field" in {

    }

    "splits a set into two sets if a tile is removed" in {

    }
  }
}
