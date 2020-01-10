package de.htwg.se.rummi.controller

import de.htwg.se.rummi.Const
import de.htwg.se.rummi.model.{Grid, Player, RummiSet, Tile, _}
import org.scalatest.{Matchers, WordSpec}

class ControllerSpec extends WordSpec with Matchers {


  var playerNames: List[String] = List("patrick", "julian")

  var controller = new Controller()
  controller.initGame(playerNames)

  val g9 = new Tile(9, GREEN)
  val g10 = new Tile(10, GREEN)
  val g8 = new Tile(8, GREEN)
  val g11 = new Tile(11, GREEN)
  val g12 = new Tile(12, GREEN)
  val g13 = new Tile(13, GREEN)

  "When the game starts a new Game " should {
    "be initiated " in {
      controller.currentSets should be(Nil)
      controller.tilesMovedFromRackToGrid should be(Nil)
    }

    "and should tell you who the first active Player is " in {
      controller.activePlayer should be(Player(playerNames(0)))
    }
  }

  "The first play is either to draw a card or to play 30+ valid points " should {
    "return false and publish a statusmessage if there are 29 or less pts played" in {

    }

    "return true if there are 30 or more valid points played " in {
      val list = g11 :: g12 :: g13 :: Nil
      controller.setGrid(Grid(Const.GRID_ROWS, Const.GRID_COLS,
        Map.empty +
          ((1, 1) -> g11) +
          ((1, 2) -> g12) +
          ((1, 3) -> g13)))

      controller.tilesMovedFromRackToGrid = list

      val correctMove = controller.playerReachedMinLayOutPoints()
      correctMove should be(true)
    }

    "return true if a player draws and than finishes the turn " in {

    }

    "return false if a player tries to skip their turn" in {

    }

    "a player shouldn't be allowed to draw 2 tiles" in {

    }


  }

  "After a Move is made it should be the next players turn " should {
    "change player " in {
      controller.setGameState(GameState.DRAWN)
      controller.tilesMovedFromRackToGrid = Nil
      controller.switchPlayer()
      controller.activePlayer should be(Player("julian"))
    }
  }

  "Before the switch the controller checks if the playingfield is valid: " should {
    val list = g11 :: g12 :: g13 :: Nil
    val list2 = g8 :: g9 :: g11 :: Nil
    val list3 = g8 :: g9 :: g10 :: Nil
    val playingfieldSet1 = new RummiSet(list)
    val playingfieldSet2 = new RummiSet(list2)
    val playingfieldSet3 = new RummiSet(list3)

    "return false if there are wrong sets " in {
      controller.setGrid(Grid(Const.GRID_ROWS, Const.GRID_COLS,
        Map.empty +
          ((1, 1) -> g11) +
          ((1, 2) -> g8) +
          ((1, 3) -> g13))
      )

      val correctMove = controller.game.isValidField
      correctMove should be(false)
    }

    "return true if multiple sets are correct " in {
      controller.setGrid(Grid(Const.GRID_ROWS, Const.GRID_COLS,
        Map.empty +
          ((1, 1) -> g11) +
          ((1, 2) -> g12) +
          ((1, 3) -> g13) +

          ((3, 1) -> g8) +
          ((3, 2) -> g9) +
          ((3, 3) -> g10)
      ))
      val correctMove = controller.game.isValidField
      correctMove should be(false)
    }
  }

  "Players can move Tiles " should {
    "either from their rack to the grid " in {
      controller.setGrid(Grid(Const.GRID_ROWS, Const.GRID_COLS,
        Map.empty +
          ((1, 1) -> g11) +
          ((1, 2) -> g12) +
          ((1, 3) -> g13) +

          ((3, 1) -> g8) +
          ((3, 2) -> g9) +
          ((3, 3) -> g10)
      ))

      val listOfSets = controller.extractSets(controller.field)
      listOfSets.size should be(2)
      listOfSets(1).tiles(0) should be(g11)
      listOfSets(1).tiles(1) should be(g12)
      listOfSets(1).tiles(2) should be(g13)

      listOfSets(0).tiles(0) should be(g8)
      listOfSets(0).tiles(1) should be(g9)
      listOfSets(0).tiles(2) should be(g10)

    }
  }
}
