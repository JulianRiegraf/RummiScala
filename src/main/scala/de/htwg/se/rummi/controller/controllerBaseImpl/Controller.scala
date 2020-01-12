package de.htwg.se.rummi.controller.controllerBaseImpl

import java.util.NoSuchElementException

import com.google.inject.{Guice, Inject}
import de.htwg.se.rummi.controller.GameState.GameState
import de.htwg.se.rummi.controller.{ControllerInterface, GameState}
import de.htwg.se.rummi.model.fileIoComponent.FileIoInterface
import de.htwg.se.rummi.model.{RummiSet, _}
import de.htwg.se.rummi.util.UndoManager
import de.htwg.se.rummi.{Const, RummiModule}

import scala.swing.event.Event


class Controller @Inject()() extends ControllerInterface {

  var currentSets: List[RummiSet] = Nil
  private var gameState: GameState = GameState.WAITING
  var tilesMovedFromRackToGrid: List[Tile] = Nil
  private val undoManager = new UndoManager
  val injector = Guice.createInjector(new RummiModule)
  val fileIo = injector.getInstance(classOf[FileIoInterface])

  var game: Game = Game(Nil)

  def initGame(): Unit = {
    val players = game.playerNames
    initGame(players)
  }

  def initGame(playerNames: List[String]): Unit = {
    gameState = GameState.WAITING
    currentSets = Nil
    tilesMovedFromRackToGrid = Nil
    game.activePlayerIndex = 0

    players.foreach(p => {
      p.inFirstRound = true
      p.points = 0
    })

    game = Game(playerNames)
    publish(new GameStateChanged)
    publish(new PlayerSwitchedEvent)
  }

  def save(): String = {
    fileIo.save(game)
  }

  def getGameState: GameState = {
    gameState
  }

  def setGameState(g: GameState): Unit = {
    gameState = g
    publish(new GameStateChanged)
  }

  def field: Grid = {
    game.grid
  }

  def players: List[Player] = {
    game.players
  }

  def undo: Unit = {
    undoManager.undoStep
    publish(new FieldChangedEvent)

  }

  def redo: Unit = {
    undoManager.redoStep
    publish(new FieldChangedEvent)
  }


  def activePlayer: Player = {
    players(game.activePlayerIndex)
  }

  // finish
  def switchPlayer(): Unit = {
    if (!(Set(GameState.DRAWN, GameState.VALID).contains(gameState))) {
      return
    }

    if (tilesMovedFromRackToGrid.size > 0) {
      activePlayer.inFirstRound = false
    }

    game.activePlayerIndex = game.activePlayerIndex + 1
    if (game.activePlayerIndex >= players.size) {
      game.activePlayerIndex = 0
    }

    currentSets = extractSets(field)

    // check if playingfield is valid
    setGameState(GameState.WAITING)
    publish(new GameStateChanged)
    tilesMovedFromRackToGrid = Nil
    publish(new PlayerSwitchedEvent)
  }

  def rackOfActivePlayer: Grid = getRack(activePlayer)

  def getRack(player: Player): Grid = {
    game.racks.find(x => x._1 == player) match {
      case Some(t) => t._2
      case None => {
        println("No Rack of " + player.name)
        throw new NoSuchElementException
      }
    }
  }

  def setGrid(newGrid: Grid) = game.grid = newGrid

  def setRack(newRack: Grid) = {
    game.racks = game.racks + (activePlayer -> newRack)
  }

  /**
    * Did player reached minimum score to get out?
    * All sets which the user builds or appends to do count.
    *
    * @return true if player reached minimum score
    */
  def playerReachedMinLayOutPoints(): Boolean = {
    val sumOfFirstMove = extractSets(field)
      .filter(x => x.tiles.toSet
        .intersect(tilesMovedFromRackToGrid.toSet).size > 0)
      .map(x => x.getPoints()).sum

    if (sumOfFirstMove < Const.MINIMUM_POINTS_FIRST_ROUND) {
      return false
    }

    true
  }

  def extractSets(field: Grid): List[RummiSet] = {
    var sets: List[RummiSet] = Nil

    field.tiles.groupBy(x => x._1._1).map(x => x._2).foreach(map => {
      var list = map.map(x => (x._1._2, x._2)).toList.sortBy(x => x._1)

      while (!list.isEmpty) {
        var tiles: List[Tile] = List.empty
        tiles = list.head._2 :: tiles
        while (list.find(x => x._1 == list.head._1 + 1).isDefined) {
          list = list.drop(1)
          tiles = list.head._2 :: tiles
        }
        sets = new RummiSet(tiles.reverse) :: sets
        list = list.drop(1)
      }
    })
    sets
  }

  /**
    * Check if all RummiSets on the field are valid.
    *
    * @return true if all sets are valid.
    */
  private def validateField(): Boolean = {
    var valid = true
    for (s <- extractSets(field)) {
      if (s.isValidRun() == false && s.isValidGroup() == false) {
        valid = false
      }
    }
    if (game.isValidField != valid) {
      game.isValidField = valid
      publish(new ValidStateChangedEvent)
    }
    valid
  }

  /**
    * Draw: If the player can not place a stone on the field, he must take a stone from the stack of covered stones.
    */
  def draw(): Unit = {

    if (gameState == GameState.DRAWN) {
      return
    }

    val newTile = game.coveredTiles.head
    game.coveredTiles = game.coveredTiles.filter(x => x != newTile)

    // get the current rack from the player
    val oldRack = game.racks.find(x => x._1 == activePlayer) match {
      case Some(r) => r._2
      case None => throw new NoSuchElementException("No rack for player '" + activePlayer + "'.")
    }

    // create a new rack with the tiles from the old one plus the newly drawn one
    val newRack = oldRack.getFreeField() match {
      case Some(freeField) => Grid(Const.RACK_ROWS, Const.RACK_COLS, oldRack.tiles + (freeField -> newTile))
      case None => throw new NoSuchElementException("No space in rack left.")
    }

    setRack(newRack)

    setGameState(GameState.DRAWN)
    publish(new FieldChangedEvent)
  }


  private def moveTileImpl(gridFrom: Grid, gridTo: Grid, tile: Tile, newRow: Int, newCol: Int): (Grid, Grid) = {
    gridFrom.getTilePosition(tile) match {
      case Some(x) => {
        if (gridTo == gridFrom) {
          // tile is moved within the same grid
          val tiles = gridFrom.tiles - (x) + ((newRow, newCol) -> tile)
          (gridFrom.copy(tiles), gridTo.copy(tiles))
        } else {
          (gridFrom.copy(gridFrom.tiles - (x)), gridTo.copy(gridTo.tiles + ((newRow, newCol) -> tile)))
        }
      }
      case None => throw new NoSuchElementException("Tile not found in rack.")
    }
  }

  def moveTile(gridFrom: Grid, gridTo: Grid, tile: Tile, newRow: Int, newCol: Int): Unit = {
    undoManager.doStep(new MoveTileCommand(gridFrom, gridTo, tile, newRow, newCol, this))
    publish(new FieldChangedEvent)
  }

  /**
    * Move a tile.
    *
    * @param gridFrom The grid, which currently holds the tile
    * @param gridTo   The grid, the tile should be moved to
    * @param tile     The tile to move
    * @param newRow   The row of the new position
    * @param newCol   The col of the new position
    * @return the updated Grids
    */
  override def updateGrids(gridFrom: Grid, gridTo: Grid, tile: Tile, newRow: Int, newCol: Int): (Grid, Grid) = {

    val (f, t): (Grid, Grid) = moveTileImpl(gridFrom, gridTo, tile, newRow, newCol)

    if ((gridFrom == field) && (gridTo == getRack(activePlayer))) {
      setRack(t)
      setGrid(f)
      tilesMovedFromRackToGrid = tilesMovedFromRackToGrid.filter(x => x != tile)
    }

    if ((gridFrom == field) && (gridTo == field)) {
      setGrid(f)
    }

    if ((gridFrom == getRack(activePlayer)) && (gridTo == field)) {
      setRack(f)
      setGrid(t)
      tilesMovedFromRackToGrid = tilesMovedFromRackToGrid :+ tile
    }

    if ((gridFrom == getRack(activePlayer)) && (gridTo == getRack(activePlayer))) {
      setRack(t)
    }
    setGameStateAfterMoveTile()
    (f, t)
  }

  private def setGameStateAfterMoveTile() = {

    if (tilesMovedFromRackToGrid.size == 0) {
      setGameState(GameState.WAITING)
    } else if (validateField()) {
      if (activePlayer.inFirstRound) {
        if (playerReachedMinLayOutPoints()) {
          setGameState(GameState.VALID)
        } else {
          setGameState(GameState.TO_LESS)
        }
      } else {
        if (getRack(activePlayer).tiles.size == 0) {
          setGameState(GameState.WON)
        } else {
          setGameState(GameState.VALID)
        }
      }
    } else {
      setGameState(GameState.INVALID)
    }
  }

  def sortRack(): Unit = {
    val sortedRack = sortRack(getRack(activePlayer))
    setRack(sortedRack)
    publish(new FieldChangedEvent)
  }

  private def sortRack(rack: Grid): Grid = {
    var tilesByColor = rack.tiles.map(x => x._2)
      .groupBy(x => x.colour)
    while (tilesByColor.size > Const.RACK_ROWS) {
      // combine colors if there are to many
      val keyOfFirstElement = tilesByColor.keys.toList(0)
      val keyOfSecondElement = tilesByColor.keys.toList(1)
      val elements = tilesByColor(keyOfFirstElement) ++ tilesByColor(keyOfSecondElement)
      tilesByColor = tilesByColor + (keyOfSecondElement -> elements)
      tilesByColor = tilesByColor - tilesByColor.keys.toList(0)
    }
    var newMap: Map[(Int, Int), Tile] = Map.empty
    var row = 1
    tilesByColor.map(x => x._2.toList).foreach(listOfTiles => {
      var col = 1
      listOfTiles.sortBy(t => t.number).foreach(t => {
        newMap = newMap + ((row, col) -> t)
        col += 1
      })
      row += 1
    })
    Grid(Const.RACK_ROWS, Const.RACK_COLS, newMap)
  }

  def moveTile(from: String, to: String): Unit = {
    val (f, t) = coordsToFields(from, to).getOrElse(throw new NoSuchElementException("No such field."))

    if (f._1 <= Const.GRID_ROWS) {
      val tile = field.getTileAt(f._1, f._2).
        getOrElse({
          println("There is no tile on field " + from)
          return
        })
      if (t._1 <= Const.GRID_ROWS) {
        moveTile(field, field, tile, t._1, t._2)
      } else {
        moveTile(field, rackOfActivePlayer, tile, t._1 - Const.GRID_ROWS, t._2)
      }
    } else {
      val tile = rackOfActivePlayer.getTileAt(f._1 - Const.GRID_ROWS, f._2).
        getOrElse({
          println("There is no tile on field " + from)
          return
        })
      if (t._1 <= Const.GRID_ROWS) {
        moveTile(rackOfActivePlayer, field, tile, t._1, t._2)
      } else {
        moveTile(rackOfActivePlayer, rackOfActivePlayer, tile, t._1 - Const.GRID_ROWS, t._2)
      }
    }
  }

  def toColNumber(col: Char): Option[Int] = {
    val ret = col - 65
    if (ret >= 0 && ret <= Const.GRID_COLS) {
      return Some(ret + 1)
    }
    None
  }

  def coordsToFields(from: String, to: String): Option[((Int, Int), (Int, Int))] = {
    val fromChars = from.toList
    val toChars = to.toList

    val toRow: Int = toChars.filter(x => x.isDigit).mkString("").toInt
    val fromRow: Int = fromChars.filter(x => x.isDigit).mkString("").toInt

    val fromCol: Int = toColNumber(fromChars(0).charValue()) match {
      case Some(c) => c
      case None => {
        return None
      }
    }
    val toCol: Int = toColNumber(toChars(0).charValue()) match {
      case Some(c) => c
      case None =>
        return None
    }
    Some((fromRow, fromCol), (toRow, toCol))
  }
}

case class PlayerSwitchedEvent() extends Event

//case class RackChangedEvent() extends Event

case class ValidStateChangedEvent() extends Event

case class FieldChangedEvent() extends Event

case class GameStateChanged() extends Event

case class WinEvent(winningPlayer: Player) extends Event
