package de.htwg.se.rummi.model


import de.htwg.se.rummi.model.RummiColour._
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random

class RummiSetSpec extends WordSpec with Matchers {

  "A Group is made from three or four same-value tiles in distinct colors. A valid Group " should {

    val greenTile1 = new Tile(1, GREEN)
    val blueTile1 = new Tile(1, BLUE)
    val yellowTile1 = new Tile(1, YELLOW)
    val joker: Tile = new Tile(-1, GREEN, true)
    val joker2: Tile = new Tile(-1, GREEN, true)

    val group1 = new RummiSet(greenTile1 :: blueTile1 :: yellowTile1 :: Nil)
    val group2 = new RummiSet(greenTile1 :: joker :: blueTile1 :: Nil)
    val group3 = new RummiSet(greenTile1 :: joker :: blueTile1 :: yellowTile1 :: Nil)
    val group4 = new RummiSet(greenTile1 :: blueTile1 :: yellowTile1 :: Nil)
    val group5 = new RummiSet(joker :: joker2 :: blueTile1 :: yellowTile1 :: Nil)
    val group6 = new RummiSet(joker :: blueTile1 :: yellowTile1 :: joker2 :: Nil)

    "return true " in {
      group1.isValidGroup() should be(true)
    }

    "return true with a joker in the middle" in {
      group2.isValidGroup() should be (true)
    }

    "return true with 4 pieces and a joker" in {
      group3.isValidGroup() should be (true)
    }

    "give the correct amount of points" in {
      group4.getPoints() should be(3)
    }

    "return true with 4 pieces and 2 joker" in {
      group5.isValidGroup() should be (true)
    }

    "return true with 4 pieces and 2 jokers" in {
      group6.isValidGroup() should be (true)
    }
  }

  "A run is composed of three or more, same-colored tiles, in consecutive number order. A Run" when {

    val g1 = new Tile(1, GREEN)
    val g2 = new Tile(2, GREEN)
    val g3 = new Tile(3, GREEN)
    val g4 = new Tile(4, GREEN)
    val b3 = new Tile(3, BLUE)
    val g11 = new Tile(11, GREEN)
    val g12 = new Tile(12, GREEN)
    val g13 = new Tile(13, GREEN)

    "to be constructed" should {
      "be return true if valid" in {

        val run1 = new RummiSet(g1 :: g2 :: g3 :: Nil)
        run1.isValidRun() should be(true)

        val run2 = new RummiSet(g1 :: g2 :: b3 :: Nil)
        run2.isValidRun() should be(false)
      }

      "be valid with shuffeld tiles" in {

        var list = g1 :: g2 :: g3 :: Nil
        list = Random.shuffle(list)

        val run1 = new RummiSet(list)
        run1.isValidRun() should be(true)

      }

      "be valid with a jocker at the end" in {
        val joker: Tile = new Tile(-1, GREEN, true)
        val list = g1 :: g2 :: g3 :: joker :: Nil

        val run = new RummiSet(list)
        run.isValidRun() should be(true)
      }

      "be invalid with a jocker at the end" in {
        val joker: Tile = new Tile(-1, GREEN, true)
        val list = g11 :: g12 :: g13 :: joker :: Nil

        val run = new RummiSet(list)
        run.isValidRun() should be(false)
      }

      "be valid with a jocker at the beginning" in {
        val joker: Tile = new Tile(-1, GREEN, true)
        val list = joker :: g2 :: g3 :: Nil

        val run = new RummiSet(list)
        run.isValidRun() should be(true)
      }

      "be invalid with a jocker at the beginning" in {
        val joker: Tile = new Tile(-1, GREEN, true)
        val list = joker :: g1 :: g2 :: g3 :: Nil

        val run = new RummiSet(list)
        run.isValidRun() should be(false)
      }

      "be invalid with a jocker at the wrong place" in {
        val joker: Tile = new Tile(-1, GREEN, true)
        val list = joker :: g2 :: g4 :: Nil

        val run = new RummiSet(list)
        run.isValidRun() should be(false)
      }


      "be return the correct number of points" in {
        var list = g1 :: g2 :: g3 :: Nil

        val run1 = new RummiSet(list)
        run1.getPoints() should be(6)
      }

      "be return the correct number of points with a joker at the beginning" in {
        val joker: Tile = new Tile(-1, GREEN, true)
        var list = joker :: g2 :: g3 :: Nil

        val run1 = new RummiSet(list)
        run1.getPoints() should be(6)
      }

      "be return the correct number of points with two jokers at the beginning" in {
        val joker: Tile = new Tile(-1, GREEN, true)
        val joker2: Tile = new Tile(-1, GREEN, true)
        var list = joker :: joker2 :: g3 :: Nil

        val run1 = new RummiSet(list)
        run1.getPoints() should be(6)
      }

      "be return the correct number of points with two jokers at the end" in {
        val joker: Tile = new Tile(-1, GREEN, true)
        val joker2: Tile = new Tile(-1, GREEN, true)
        var list = g1 :: g2 :: joker :: joker2 :: Nil

        val run1 = new RummiSet(list)
        run1.getPoints() should be(10)
      }

      "be return the correct number of points with two jokers random" in {
        val joker: Tile = new Tile(-1, GREEN, true)
        val joker2: Tile = new Tile(-1, GREEN, true)
        var list = joker :: g11 :: joker2 :: g13 :: Nil

        val run1 = new RummiSet(list)
        run1.getPoints() should be(46)
      }

      "be return the correct number of points with two jokers at the end and the beginning" in {
        val joker: Tile = new Tile(-1, GREEN, true)
        val joker2: Tile = new Tile(-1, GREEN, true)
        var list = joker :: g2 :: g3 :: joker2 :: Nil

        val run1 = new RummiSet(list)
        run1.getPoints() should be(10)
      }

      "be return the correct number of points with two jokers in the middle" in {
        val joker: Tile = new Tile(-1, GREEN, true)
        val joker2: Tile = new Tile(-1, GREEN, true)
        var list = g1 :: joker :: joker2 :: g4 :: Nil

        val run1 = new RummiSet(list)
        run1.getPoints() should be(10)
      }

      "be return the correct number of points with a joker at the end" in {
        val joker: Tile = new Tile(-1, GREEN, true)
        var list = g1 :: g2 :: g3 :: joker :: Nil

        val run1 = new RummiSet(list)
        run1.getPoints() should be(10)
      }

      "joker at wrong position should be false" in {
        val joker: Tile = new Tile(-1, GREEN, true)
        var list = joker :: g1  :: g3 :: Nil

        val run1 = new RummiSet(list)
        run1.isValidRun() should be(false)
      }

      "should append a tile, if added to the right" in {
        val set = new RummiSet(List(g1))
        set.add(g2, Ending.RIGHT)
        set.tiles(0) should be(g1)
        set.tiles(1) should be(g2)
      }

      "should add a tile to the head, if added to the left" in {
        val set = new RummiSet(List(g1))
        set.add(g2, Ending.LEFT)
        set.tiles(0) should be(g2)
        set.tiles(1) should be(g1)
      }
    }
  }
}