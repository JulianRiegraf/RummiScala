package de.htwg.se.rummi.model


import de.htwg.se.rummi.model.RummiColors._
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random

class RummiSetSpec extends WordSpec with Matchers {

  "A group is made from three or four same-value tiles in distinct colors. A valid Group " should {

    val greenTile1 = new Tile(1, GREEN)
    val blueTile1 = new Tile(1, BLUE)
    val yellowTile1 = new Tile(1, YELLOW)


    "return true" in {
      val group1 = new RummiSet(greenTile1 :: blueTile1 :: yellowTile1 :: Nil)
      group1.isValidGroup() should be(true)
    }

    "give the correct amount of points" in {
      val group1 = new RummiSet(greenTile1 :: blueTile1 :: yellowTile1 :: Nil)
      group1.getPoints() should be(3)
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

     /* "be invalid with a jocker at the end" in {
        val joker: Tile = new Tile(-1, GREEN, true)
        val list = g11 :: g12 :: g13 :: joker :: Nil

        val run = new RummiSet(list)
        run.isValidRun() should be(false)
      }*/

      "be valid with a jocker at the beginning" in {
        val joker: Tile = new Tile(-1, GREEN, true)
        val list = joker :: g2 :: g3 :: Nil

        val run = new RummiSet(list)
        run.isValidRun() should be(true)
      }

    /*  "be invalid with a jocker at the beginning" in {
        val joker: Tile = new Tile(-1, GREEN, true)
        val list = joker :: g1 :: g2 :: g3 :: Nil

        val run = new RummiSet(list)
        run.isValidRun() should be(false)
      }*/

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

      "be return the correct number of points with two jokers at the beginning" in {
        val joker: Tile = new Tile(-1, GREEN, true)
        val joker2: Tile = new Tile(-1, GREEN, true)
        var list = g1 :: g2 :: joker :: joker2 :: Nil

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
    }
  }
}