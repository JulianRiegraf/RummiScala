package de.htwg.se.rummi.controller.controllerBaseImpl

import com.google.inject.Guice
import de.htwg.se.rummi.{Const, RummiModule}
import de.htwg.se.rummi.controller.GameState
import de.htwg.se.rummi.model.fileIoComponent.jsonImpl.JsonFileIo
import de.htwg.se.rummi.model.fileIoComponent.xmlFileIo.XmlFileIo
import de.htwg.se.rummi.model.fileIoComponent.{FileIoInterface, xmlFileIo}
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
    "return false and publish a status message if there are 29 or less pts played" in {
      val correctMove = controller.playerReachedMinLayOutPoints()
      correctMove should be(false)
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

    "return the rack of a specific player " in {
      controller.getRack(controller.activePlayer) shouldBe
        controller.game.racks(controller.activePlayer)

      an[NoSuchElementException] should be thrownBy
        controller.getRack(Player("invalid player"))
    }

    "set the rack of the active player" in {
      val theNewRack = Grid(4, 13, Map.empty)
      controller.setRack(theNewRack)
      controller.getRack(controller.activePlayer) shouldBe theNewRack
    }

    "translate coordinates from A1 to tuple(1,1)" in {
      controller.coordsToFields("A9", "A1").get shouldBe((9, 1), (1, 1))
      val outOfBoundsCoordinate = "Z324"
      controller.coordsToFields(outOfBoundsCoordinate, "A1") shouldBe None
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

    "return false if there are wrong set" in {
      controller.setGrid(Grid(Const.GRID_ROWS, Const.GRID_COLS,
        Map.empty +
          ((1, 1) -> g11) +
          ((1, 2) -> g8) +
          ((1, 4) -> g13))
      )

      controller.moveTile("A3", "F5");
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

  "Players draw tiles" should {
    "take a tile from the stack and adds it to the players rack" in {
      val tile = controller.game.coveredTiles(0)
      var rack = controller.getRack(controller.activePlayer)
      rack.tiles.values.toList.contains(tile) shouldBe false
      controller.draw()
      rack = controller.getRack(controller.activePlayer)
      rack.tiles.values.toList.contains(tile) shouldBe true
    }

    "throw exception if rack is full" in {
      for(_ <- 0 to (2*13*4) - (2*14+1))
        {
          controller.draw()
          controller.switchPlayer()
        }
      a[NoSuchElementException] should be thrownBy controller.draw()
    }

    "can sort tiles by color and number" in {
      val g1 = Tile(1, GREEN)
      val g5 = Tile(5, GREEN)
      val b1 = Tile(1, BLUE)
      val y1 = Tile(1, YELLOW)
      val r1 = Tile(1, RED)


      controller.setRack(Grid(Const.GRID_ROWS, Const.GRID_COLS,
        Map.empty +
          ((1, 1) -> g1) +
          ((1, 2) -> g5) +
          ((1, 7) -> b1) +
          ((5, 3) -> r1) +
          ((1, 3) -> y1)
      ))

      controller.sortRack()
      val rack = controller.getRack(controller.activePlayer)
      rack.tiles shouldBe Map.empty +
        ((1, 1) -> r1) +
        ((2, 1) -> g1) +
        ((2, 2) -> g5) +
        ((3, 1) -> y1) +
        ((4, 1) -> b1)
    }

    "can sort tiles by color with 5 colors" in {
      val g1 = Tile(1, GREEN)
      val g5 = Tile(5, WHITE)
      val b1 = Tile(1, BLUE)
      val y1 = Tile(1, YELLOW)
      val r1 = Tile(1, RED)


      controller.setRack(Grid(Const.GRID_ROWS, Const.GRID_COLS,
        Map.empty +
          ((1, 1) -> g1) +
          ((1, 2) -> g5) +
          ((1, 7) -> b1) +
          ((5, 3) -> r1) +
          ((1, 3) -> y1)
      ))
    }
  }

  "Players can move Tiles " should {
    "either from their rack to the field " in {
      controller.setRack(Grid(Const.GRID_ROWS, Const.GRID_COLS,
        Map.empty +
          ((1, 1) -> g11)
      ))

      val rack = controller.getRack(controller.activePlayer)
      controller.moveTile(rack, controller.field, g11, 1, 2)
      controller.field.getTileAt(1, 2).get shouldBe g11
    }

    "or within the rack " in {
      controller.setRack(Grid(Const.GRID_ROWS, Const.GRID_COLS,
        Map.empty +
          ((1, 1) -> g11)
      ))

      var rack = controller.getRack(controller.activePlayer)

      controller.moveTile(rack, rack, g11, 1, 2)

      rack = controller.getRack(controller.activePlayer)
      rack.getTileAt(1, 2).get shouldBe g11
    }

    "from field to rack" in {
      controller.setRack(Grid(Const.GRID_ROWS, Const.GRID_COLS,
        Map.empty +
          ((1, 1) -> g11)
      ))
      var rack = controller.getRack(controller.activePlayer)
      controller.moveTile(rack, controller.field, g11, 1, 1)

      controller.moveTile("A1", "A9")

      rack = controller.getRack(controller.activePlayer)
      rack.getTileAt(1, 1).get shouldBe g11
    }

    "within the field" in {
      controller.setRack(Grid(Const.GRID_ROWS, Const.GRID_COLS,
        Map.empty +
          ((1, 1) -> g11)
      ))
      var rack = controller.getRack(controller.activePlayer)
      controller.moveTile(rack, controller.field, g11, 1, 1)

      controller.moveTile("A1", "C1")


      val field = controller.field
      field.getTileAt(1, 3).get shouldBe g11
    }

    "by their variables" in {
      controller.setRack(Grid(Const.GRID_ROWS, Const.GRID_COLS,
        Map.empty +
          ((1, 1) -> g11)
      ))

      val rack = controller.getRack(controller.activePlayer)
      controller.moveTile("A9", "B2") // A9 = (1,1) in rack; B2 = (1,2) in field
      controller.field.getTileAt(1, 2).get shouldBe g11
    }
  }

  "On save, controller " should {
    "return the serialized game in json format" in {
      controller.initGame("p1" :: "p2" :: Nil)
      val fileIo = new JsonFileIo();
      val json = fileIo.save(controller.game)
      json shouldNot(be(""))
    }

    "return the serialized game in xml format" in {

      controller.initGame("p1" :: "p2" :: Nil)
      val fileIo = new XmlFileIo();
      val xml = fileIo.save(controller.game)
      xml shouldNot(be(""))
    }
  }

  "controller undo" should {
    "undo the last tile movement" in {
      controller.setRack(Grid(Const.GRID_ROWS, Const.GRID_COLS,
        Map.empty +
          ((1, 1) -> g11)
      ))

      var rack = controller.getRack(controller.activePlayer)

      controller.moveTile(rack, rack, g11, 1, 2)

      rack = controller.getRack(controller.activePlayer)
      rack.getTileAt(1, 2).get shouldBe g11

      controller.undo

      rack = controller.getRack(controller.activePlayer)
      rack.getTileAt(1, 1).get shouldBe g11
    }

    "redo the last tile movement" in {
      controller.setRack(Grid(Const.GRID_ROWS, Const.GRID_COLS,
        Map.empty +
          ((1, 1) -> g11)
      ))

      var rack = controller.getRack(controller.activePlayer)

      controller.moveTile(rack, rack, g11, 1, 2)

      rack = controller.getRack(controller.activePlayer)
      rack.getTileAt(1, 2).get shouldBe g11

      controller.undo
      controller.redo

      rack = controller.getRack(controller.activePlayer)
      rack.getTileAt(1, 2).get shouldBe g11
    }
  }
}
