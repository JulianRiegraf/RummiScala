package de.htwg.se.rummi.controller

import de.htwg.se.rummi.model.RummiColor.GREEN
import de.htwg.se.rummi.model.{Player, RummiSet, Tile}
import org.scalatest.{Matchers, WordSpec}


class ControllerSpec extends WordSpec with Matchers {

  var playerNames: List[String] = List("patrick", "julian")

  var controller = new Controller(playerNames)
  controller.initGame()

  val g9 = new Tile(9, GREEN)
  val g10 = new Tile(10, GREEN)
  val g8 = new Tile(8, GREEN)
  val g11 = new Tile(11, GREEN)
  val g12 = new Tile(12, GREEN)
  val g13 = new Tile(13, GREEN)

  "When the game starts a new Game " should {
    "be initiated " in{
      controller.statusMessage should be ("")
      controller.hasDrawn should be (false)
      controller.currenSets should be (Nil)
      controller.tilesMovedFromRacktoGrid should be (Nil)
      controller.firstMoveList should be (controller.players)
    }

    "and should tell you who the first active Player is " in {
      controller.getActivePlayer should be (Player(playerNames(0)))
    }
  }

  "The first play is either to draw a card or to play 30+ valid points " should {
    "return false and publish a statusmessage if there are 29 or less pts played" in {
      val noMove = controller.isValid()
      noMove should be (false)
      controller.statusMessage should be ("Du musst in deinem ersten Zug 30 oder mehr Punkte legen.")
    }

    "return true if there are 30 or more valid points played " in {
      val list = g11 :: g12 :: g13 :: Nil
      val playingfieldSet = new RummiSet(list)
      controller.playingfield.sets = List(playingfieldSet)
      controller.tilesMovedFromRacktoGrid = list

      val correctMove = controller.isValid()
      correctMove should be (true)
    }

    "return true if a player draws and than finishes the turn " in{
      controller.hasDrawn = true
      controller.tilesMovedFromRacktoGrid = Nil
      var pass = controller.isValid()
      pass should be (true)
    }

    "return false if a player tries to skip their turn" in {
      controller.hasDrawn = false
      controller.tilesMovedFromRacktoGrid = Nil
      var noMove = controller.isValid()
      noMove should be (false)
      controller.statusMessage should be("Du musst ziehen oder Steine legen...")
    }

    "a player shouldn't be allowed to draw 2 tiles" in {
      controller.hasDrawn = false
      controller.draw()
      controller.hasDrawn should be (true)
      controller.draw()
      controller.statusMessage should be ("Du darfst nur einmal ziehen, du Papnase.")
    }

    "also a player shouldn't be allowed to draw if he already played tiles. " in {
      controller.hasDrawn = false
      controller.tilesMovedFromRacktoGrid = g10 :: g11 :: g12 :: Nil
      controller.draw()
      controller.statusMessage should be ("Du host scho glegt, du Zipfelklatscher.")
    }
  }

  "After a Move is made it should be the next players turn " should{
    "change player " in {
      controller.hasDrawn = true
      controller.tilesMovedFromRacktoGrid = Nil
      controller.switchPlayer()
      controller.getActivePlayer should be (Player("julian"))
    }
  }

  "Before the switch the controller checks if the playingfield is valid: " should {
    val list = g11 :: g12 :: g13 :: Nil
    val list2 = g8 :: g9 :: g11 :: Nil
    val list3 = g8 :: g9 :: g10 :: Nil
    val playingfieldSet1 = new RummiSet(list)
    val playingfieldSet2 = new RummiSet(list2)
    val playingfieldSet3 = new RummiSet(list3)

    "return false if there are wrong sets " in{
      controller.playingfield.sets = List(playingfieldSet1, playingfieldSet2)
      val wrongMove = controller.isValid()
      wrongMove should be (false)
      controller.statusMessage should be ("Dein Spielfeld hat fehler, du Pisser!")
    }

    "return true if multiple sets are correct " in {
      controller.playingfield.sets = List(playingfieldSet1, playingfieldSet3)
      val correctMove = controller.isValid()
      correctMove should be (false)
    }
  }

  "Players can move Tiles " should {
    "either from their rack to the grid " in {

    }
  }

}
