package de.htwg.se.rummi.controller

import java.util.NoSuchElementException

import de.htwg.se.rummi.Const
import de.htwg.se.rummi.model.{RummiSet, _}

import scala.swing.Publisher
import scala.swing.event.Event

class Controller(playerNames: List[String]) extends Publisher {

  val number_of_number_tiles = 104
  val number_of_joker_tiles = 2
  val lowest_number = 1
  val highest_number = 13

  var statusMessage: String = ""
  var hasDrawn = false
  var currentSets: List[RummiSet] = Nil

  var tilesMovedFromRackToGrid: List[Tile] = Nil

  val playingfield = new Playingfield()
  val players = playerNames.map(x => new Player(x))
  var firstMoveList = players
  var isValidField = false
  private var activePlayerIndex: Int = 0

  def initGame() = {

    statusMessage = ""
    hasDrawn = false
    currentSets = Nil
    tilesMovedFromRackToGrid = Nil
    firstMoveList = players
    activePlayerIndex = 0

    playingfield.generateNewGame(players)

    publish(new StatusMessageChangedEvent)
    publish(new PlayerSwitchedEvent)
  }

  def activePlayer: Player = {
    players(activePlayerIndex)
  }

  // finish
  def switchPlayer(): Unit = {

    if (playerCanFinish() == false) {
      return
    }

    activePlayerIndex = activePlayerIndex + 1
    if (activePlayerIndex >= players.size) {
      activePlayerIndex = 0
    }

    hasDrawn = false
    currentSets = extractSets(field)

    // check if playingfield is valid
    statusMessage = ""
    publish(new StatusMessageChangedEvent)
    tilesMovedFromRackToGrid = Nil
    publish(new PlayerSwitchedEvent)
  }

  def field: Grid = {
    playingfield.grid
  }

  def setGrid(newGrid: Grid) = playingfield.grid = newGrid

  def getRack(player: Player): Grid = {
    playingfield.racks.find(x => x._1 == player) match {
      case Some(t) => t._2
      case None => {
        println("No Rack of " + player.name)
        throw new NoSuchElementException
      }
    }
  }

  def setRack(newRack: Grid) = {
    playingfield.racks = playingfield.racks + (activePlayer -> newRack)
  }

  /**
    * Did player reached minimum score to get out?
    *
    * @return true if player reached minimum score
    */
  def checkMinimumScoreInFirstMove(): Boolean = {
    if (firstMoveList.contains(activePlayer)) {
      val sumOfFirstMove = extractSets(field)
        .filter(x => !currentSets.contains(x))
        .map(x => x.getPoints()).sum

      if (sumOfFirstMove < Const.MINIMUM_POINTS_FIRST_ROUND) {
        return false
      }

      firstMoveList = firstMoveList.drop(firstMoveList.indexOf(activePlayer))
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

  def checkWinCon(): Boolean = {
    val activeRack = getRack(activePlayer)
    if (activeRack.size == 0) true
    else false
  }

  def playerCanFinish(): Boolean = {

    if (hasDrawn) {
      return true
    }

    if (tilesMovedFromRackToGrid.size <= 0) {
      statusMessage = "Please place at least one stone or draw."
      publish(new StatusMessageChangedEvent)
      return false
    }

    if (!validateField()) {
      return false
    }

    if (!checkMinimumScoreInFirstMove()) {
      statusMessage = "You have to score 30 or more points on your first turn."
      publish(new StatusMessageChangedEvent)
      return false
    }

    if (checkWinCon()) {
      //Game End
      statusMessage = "You won! °_°"
      publish(new StatusMessageChangedEvent)
      publish(new WinEvent(activePlayer))
    }

    true
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
    if (isValidField != valid) {
      isValidField = valid
      publish(new ValidStateChangedEvent)
    }
    valid
  }

  /**
    * Draw: If the player can not place a stone on the field, he must take a stone from the stack of covered stones.
    */
  def draw(): Unit = {

    if (hasDrawn) {
      statusMessage = "You can draw only once."
      publish(new StatusMessageChangedEvent)
      return
    }

    if (tilesMovedFromRackToGrid.size != 0) {
      statusMessage = "You have already placed a stone on the field."
      publish(new StatusMessageChangedEvent)
      return
    }

    val newTile = playingfield.coveredTiles.head
    playingfield.coveredTiles = playingfield.coveredTiles.filter(x => x != newTile)

    // get the current rack from the player
    val oldRack = playingfield.racks.find(x => x._1 == activePlayer) match {
      case Some(r) => r._2
      case None => throw new NoSuchElementException("No rack for player '" + activePlayer + "'.")
    }

    // create a new rack with the tiles from the old one plus the newly drawn one
    val newRack = oldRack.getFreeField() match {
      case Some(freeField) => Grid(Const.RACK_ROWS, Const.RACK_COLS, oldRack.tiles + (freeField -> newTile))
      case None => throw new NoSuchElementException("No space in rack left.")
    }

    setRack(newRack)

    hasDrawn = true
    publish(new FieldChangedEvent)
  }


  def moveTile(gridFrom: Grid, gridTo: Grid, tile: Tile, newRow: Int, newCol: Int): (Grid, Grid) = {
    gridFrom.getTilePosition(tile) match {
      case Some(x) => {
        if (gridTo == gridFrom) {
          // tile is moved within the same grid
          val tiles = gridFrom.tiles - (x) + ((newRow, newCol) -> tile)
          (Grid(gridFrom.ROWS, gridFrom.COLS, tiles),
            Grid(gridTo.ROWS, gridTo.COLS, tiles))
        } else {
          (Grid(gridFrom.ROWS, gridFrom.COLS, gridFrom.tiles - (x)),
            Grid(gridTo.ROWS, gridTo.COLS, gridTo.tiles + ((newRow, newCol) -> tile)))
        }
      }
      case None => throw new NoSuchElementException("Tile not found in rack.")
    }
  }

  def fieldChanged(): Unit = {
    publish(new FieldChangedEvent)
    validateField()
  }

  def moveTileWithinField(tile: Tile, newRow: Int, newCol: Int): Unit = {
    val (_, f) = moveTile(field, field, tile, newRow, newCol)
    setGrid(f)
    fieldChanged()
  }

  def moveTileWithinRack(tile: Tile, newRow: Int, newCol: Int): Unit = {
    val (_, rack) = moveTile(getRack(activePlayer), getRack(activePlayer), tile, newRow, newCol)
    setRack(rack)
    fieldChanged()
  }

  def moveTileFromRackToField(tile: Tile, newRow: Int, newCol: Int): Unit = {
    val (rack, grid) = moveTile(getRack(activePlayer), field, tile, newRow, newCol)
    setRack(rack)
    setGrid(grid)
    tilesMovedFromRackToGrid = tilesMovedFromRackToGrid :+ tile
    fieldChanged()
  }

  def moveTileFromFieldToRack(tile: Tile, newRow: Int, newCol: Int): Unit = {
    val (f, rack): (Grid, Grid) = moveTile(field, getRack(activePlayer), tile, newRow, newCol)
    setRack(rack)
    setGrid(f)
    tilesMovedFromRackToGrid = tilesMovedFromRackToGrid.filter(x => x != tile)
    fieldChanged()
  }

  def sortRack(): Unit = {
    val sortedRack = sortRack(getRack(activePlayer))
    setRack(sortedRack)
    fieldChanged()
  }

  private def sortRack(rack: Grid): Grid = {
    var tilesByColor = rack.tiles.map(x => x._2)
      .groupBy(x => x.color)
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
}

case class PlayerSwitchedEvent() extends Event

//case class RackChangedEvent() extends Event

case class ValidStateChangedEvent() extends Event

case class FieldChangedEvent() extends Event

case class StatusMessageChangedEvent() extends Event

case class WinEvent(winningPlayer: Player) extends Event